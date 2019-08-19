open! Stdune
open Import
open Fiber.O

type done_or_more_deps =
  | Done of Dep.Set.t (* Dynamic deps used by this exec call. *)
  | Need_more_deps of Dep.Set.t

type exec_context =
  { context : Context.t option
  ; purpose : Process.purpose
  }

let empty_done = Done Dep.Set.empty

let exec_run ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from prog args =
  ( match ectx.context with
  | None
   |Some { Context.for_host = None; _ } ->
    ()
  | Some ({ Context.for_host = Some host; _ } as target) ->
    let invalid_prefix prefix =
      match Path.descendant prog ~of_:prefix with
      | None -> ()
      | Some _ ->
        User_error.raise
          [ Pp.textf "Context %s has a host %s." target.name host.name
          ; Pp.textf "It's not possible to execute binary %s in it."
            (Path.to_string_maybe_quoted prog)
          ; Pp.nop
          ; Pp.text "This is a bug and should be reported upstream."
          ]
    in
    invalid_prefix (Path.relative Path.build_dir target.name);
    invalid_prefix (Path.relative Path.build_dir ("install/" ^ target.name)) );
  Process.run Strict ~dir ~env ~stdout_to ~stderr_to ~stdin_from
    ~purpose:ectx.purpose prog args

(* TODO jstaron: This is copy-paste from above. Remove duplication. *)

let exec_run_dynamic_client ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
  prog args =
  ( match ectx.context with
  | None
   |Some { Context.for_host = None; _ } ->
    ()
  | Some ({ Context.for_host = Some host; _ } as target) ->
    let invalid_prefix prefix =
      match Path.descendant prog ~of_:prefix with
      | None -> ()
      | Some _ ->
        User_error.raise
          [ Pp.textf "Context %s has a host %s." target.name host.name
          ; Pp.textf "It's not possible to execute binary %s in it."
            (Path.to_string_maybe_quoted prog)
          ; Pp.nop
          ; Pp.text "This is a bug and should be reported upstream."
          ]
    in
    invalid_prefix (Path.relative Path.build_dir target.name);
    invalid_prefix (Path.relative Path.build_dir ("install/" ^ target.name)) );
  let run_in_dune_fn = Filename.temp_file "" ".run_in_dune" in
  let response_fn = Filename.temp_file "" ".response" in
  let env =
    let value = String.concat ~sep:":" [ run_in_dune_fn; response_fn ] in
    Env.add env ~var:Dune_action.Protocol.For_dune.dune_action_env_variable
      ~value:serialized
  in
  let* () =
    Process.run Strict ~dir ~env ~stdout_to ~stderr_to ~stdin_from
      ~purpose:ectx.purpose prog args
  in
  if not (File.exists response_fn) then
    (* TODO jstaron: Pass info about rule and raise User_error here. *)
    failwith "DUNE ACTION CLIENT DIDN'T RESPOND";
  let response = Io.read_file (Path.of_string response_fn) in
  (* TODO jstaron: Parse response and build deps, rerun if needed. *)
  failwith "PARSE RESPONSE"

let exec_echo stdout_to str =
  Fiber.return (output_string (Process.Io.out_channel stdout_to) str)

let rec exec t ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from =
  match (t : Action.t) with
  | Run (Error e, _) -> Action.Prog.Not_found.raise e
  | Run (Ok prog, args) ->
    let+ () =
      exec_run ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from prog args
    in
    empty_done
  | Run_dynamic (Error e, _) -> Action.Prog.Not_found.raise e
  | Run_dynamic (Ok prog, args) ->
    exec_run_dynamic_client ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
      prog args
  | Chdir (dir, t) -> exec t ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
  | Setenv (var, value, t) ->
    exec t ~ectx ~dir ~stdout_to ~stderr_to ~stdin_from
      ~env:(Env.add env ~var ~value)
  | Redirect_out (Stdout, fn, Echo s) ->
    Io.write_file (Path.build fn) (String.concat s ~sep:" ");
    Fiber.return empty_done
  | Redirect_out (outputs, fn, t) ->
    let fn = Path.build fn in
    redirect_out ~ectx ~dir outputs fn t ~env ~stdout_to ~stderr_to ~stdin_from
  | Redirect_in (inputs, fn, t) ->
    redirect_in ~ectx ~dir inputs fn t ~env ~stdout_to ~stderr_to ~stdin_from
  | Ignore (outputs, t) ->
    redirect_out ~ectx ~dir outputs Config.dev_null t ~env ~stdout_to
      ~stderr_to ~stdin_from
  | Progn l ->
    exec_list l ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
      ~result_acc:empty_done
  | Echo strs ->
    let+ () = exec_echo stdout_to (String.concat strs ~sep:" ") in
    empty_done
  | Cat fn ->
    Io.with_file_in fn ~f:(fun ic ->
      Io.copy_channels ic (Process.Io.out_channel stdout_to));
    Fiber.return empty_done
  | Copy (src, dst) ->
    let dst = Path.build dst in
    Io.copy_file ~src ~dst ();
    Fiber.return empty_done
  | Symlink (src, dst) ->
    ( if Sys.win32 then
      let dst = Path.build dst in
      Io.copy_file ~src ~dst ()
    else
      let src =
        match Path.Build.parent dst with
        | None -> Path.to_string src
        | Some from ->
          let from = Path.build from in
          Path.reach ~from src
      in
      let dst = Path.Build.to_string dst in
      match Unix.readlink dst with
      | target ->
        if target <> src then (
          (* @@DRA Win32 remove read-only attribute needed when symlinking
            enabled *)
          Unix.unlink dst;
          Unix.symlink src dst
        )
      | exception _ -> Unix.symlink src dst );
    Fiber.return empty_done
  | Copy_and_add_line_directive (src, dst) ->
    Io.with_file_in src ~f:(fun ic ->
      Path.build dst
      |> Io.with_file_out ~f:(fun oc ->
        let fn = Path.drop_optional_build_context_maybe_sandboxed src in
        output_string oc
          (Utils.line_directive ~filename:(Path.to_string fn) ~line_number:1);
        Io.copy_channels ic oc));
    Fiber.return empty_done
  | System cmd ->
    let path, arg =
      Utils.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    let+ () =
      exec_run ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from path
        [ arg; cmd ]
    in
    empty_done
  | Bash cmd ->
    let+ () =
      exec_run ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
        (Utils.bash_exn ~needed_to:"interpret (bash ...) actions")
        [ "-e"; "-u"; "-o"; "pipefail"; "-c"; cmd ]
    in
    empty_done
  | Write_file (fn, s) ->
    Io.write_file (Path.build fn) s;
    Fiber.return empty_done
  | Rename (src, dst) ->
    Unix.rename (Path.Build.to_string src) (Path.Build.to_string dst);
    Fiber.return empty_done
  | Remove_tree path ->
    Path.rm_rf (Path.build path);
    Fiber.return empty_done
  | Mkdir path ->
    if Path.is_in_build_dir path then
      Path.mkdir_p path
    else
      Code_error.raise "Action_exec.exec: mkdir on non build dir"
        [ ("path", Path.to_dyn path) ];
    Fiber.return empty_done
  | Digest_files paths ->
    let s =
      let data =
        List.map paths ~f:(fun fn ->
          (Path.to_string fn, Cached_digest.file fn))
      in
      Digest.generic data
    in
    let+ () = exec_echo stdout_to (Digest.to_string_raw s) in
    empty_done
  | Diff ({ optional; file1; file2; mode } as diff) ->
    let remove_intermediate_file () =
      if optional then
        try Path.unlink file2 with Unix.Unix_error (ENOENT, _, _) -> ()
    in
    if Diff.eq_files diff then (
      remove_intermediate_file ();
      Fiber.return empty_done
    ) else
      let is_copied_from_source_tree file =
        match Path.extract_build_context_dir_maybe_sandboxed file with
        | None -> false
        | Some (_, file) -> Path.exists (Path.source file)
      in
      let+ () =
        Fiber.finalize
          (fun () ->
            if mode = Binary then
              User_error.raise
                [ Pp.textf "Files %s and %s differ."
                  (Path.to_string_maybe_quoted file1)
                    (Path.to_string_maybe_quoted file2)
                ]
            else
              Print_diff.print file1 file2
                ~skip_trailing_cr:(mode = Text && Sys.win32))
          ~finally:(fun () ->
            ( match optional with
            | false ->
              if
                is_copied_from_source_tree file1
                && not (is_copied_from_source_tree file2)
              then
                Promotion.File.register_dep
                  ~source_file:
                    (snd
                      (Option.value_exn
                        (Path.extract_build_context_dir_maybe_sandboxed file1)))
                  ~correction_file:(Path.as_in_build_dir_exn file2)
            | true ->
              if is_copied_from_source_tree file1 then
                Promotion.File.register_intermediate
                  ~source_file:
                    (snd
                      (Option.value_exn
                        (Path.extract_build_context_dir_maybe_sandboxed file1)))
                  ~correction_file:(Path.as_in_build_dir_exn file2)
              else
                remove_intermediate_file () );
            Fiber.return ())
      in
      empty_done
  | Merge_files_into (sources, extras, target) ->
    let lines =
      List.fold_left
        ~init:(String.Set.of_list extras)
        ~f:(fun set source_path ->
          Io.lines_of_file source_path
          |> String.Set.of_list |> String.Set.union set)
        sources
    in
    let target = Path.build target in
    Io.write_lines target (String.Set.to_list lines);
    Fiber.return empty_done

and redirect_out outputs fn t ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
  =
  let out = Process.Io.file fn Process.Io.Out in
  let stdout_to, stderr_to =
    match outputs with
    | Stdout -> (out, stderr_to)
    | Stderr -> (stdout_to, out)
    | Outputs -> (out, out)
  in
  exec t ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
  >>| fun result ->
  Process.Io.release out;
  result

and redirect_in inputs fn t ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from:_
  =
  let in_ = Process.Io.file fn Process.Io.In in
  let stdin_from =
    match inputs with
    | Stdin -> in_
  in
  exec t ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
  >>| fun result ->
  Process.Io.release in_;
  result

and merge_result (first : done_or_more_deps) (second : done_or_more_deps) =
  match (first, second) with
  | Done first, Done second -> Done (Dep.Set.union first second)
  | Need_more_deps first, Need_more_deps second ->
    Need_more_deps (Dep.Set.union first second)
  | (Need_more_deps _ as result), _
   |_, (Need_more_deps _ as result) ->
    result

and exec_list l ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from ~result_acc =
  match l with
  | [] -> Fiber.return empty_done
  | [ t ] ->
    let+ result = exec t ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from in
    merge_result result result_acc
  | t :: rest ->
    let* done_or_deps =
      let stdout_to = Process.Io.multi_use stdout_to in
      let stderr_to = Process.Io.multi_use stderr_to in
      let stdin_from = Process.Io.multi_use stdin_from in
      exec t ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
    in
    let result_acc = merge_result done_or_deps result_acc in
    exec_list rest ~ectx ~dir ~env ~stdout_to ~stderr_to ~stdin_from
      ~result_acc

let exec ~targets ~context ~env t =
  let env =
    match ((context : Context.t option), env) with
    | _, Some e -> e
    | None, None -> Env.initial
    | Some c, None -> c.env
  in
  let purpose = Process.Build_job targets in
  let ectx = { purpose; context } in
  exec t ~ectx ~dir:Path.root ~env ~stdout_to:Process.Io.stdout
    ~stderr_to:Process.Io.stderr ~stdin_from:Process.Io.stdin
