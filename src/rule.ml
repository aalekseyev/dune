open! Stdune
open Import

module Info = struct
  type t =
    | From_dune_file of Loc.t
    | Internal of Printexc.raw_backtrace
    | Source_file_copy

  let of_loc_opt = function
    | None -> Internal (Printexc.get_callstack 50)
    | Some loc -> From_dune_file loc

  let loc = function
    | From_dune_file loc -> Some loc
    | Internal _
    | Source_file_copy -> None
end

type t =
  { context  : Context.t option
  ; env      : Env.t option
  ; build    : (unit, Action.t) Build.t
  ; targets  : Path.Build.Set.t
  ; sandbox  : bool
  ; mode     : Dune_file.Rule.Mode.t
  ; locks    : Path.t list
  ; info     : Info.t
  ; dir      : Path.Build.t
  }

let make ?(sandbox=false) ?(mode=Dune_file.Rule.Mode.Standard)
      ~context ~env ?(locks=[])
      ?info
      build =
  let info = match info with
    | Some info -> info
    | None -> Info.Internal (Printexc.get_callstack 50)
  in
  let targets = Build.targets build in
  let dir =
    match Path.Build.Set.choose targets with
    | None -> begin
        match info with
        | From_dune_file loc -> Errors.fail loc "Rule has no targets specified"
        | _ -> Exn.code_error "Build_interpret.Rule.make: no targets" []
      end
    | Some x ->
      let dir = Path.Build.parent_exn x in
      if Path.Build.Set.exists targets ~f:(fun path ->
        Path.Build.(<>) (Path.Build.parent_exn path) dir)
      then begin
        match info with
        | Internal bt ->
          Exn.code_error "rule has targets in different directories"
            [ "targets", Path.Build.Set.to_sexp targets
            ; "backtrace", Sexp.Atom (Printexc.raw_backtrace_to_string bt)
            ]
        | Source_file_copy ->
          Exn.code_error "rule has targets in different directories"
            [ "targets", Path.Build.Set.to_sexp targets
            ]
        | From_dune_file loc ->
          Errors.fail loc
            "Rule has targets in different directories.\nTargets:\n%s"
            (String.concat ~sep:"\n"
               (Path.Build.Set.to_list targets |> List.map ~f:(fun p ->
                  sprintf "- %s"
                    (Path.to_string_maybe_quoted (Path.build p)))))
      end;
      dir
  in
  { context
  ; env
  ; build
  ; targets
  ; sandbox
  ; mode
  ; locks
  ; info
  ; dir
  }
