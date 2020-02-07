open! Stdune
open Import

module Info = struct
  type t =
    | From_dune_file of Loc.t
    | Internal
    | Source_file_copy

  let of_loc_opt = function
    | None -> Internal
    | Some loc -> From_dune_file loc

  let loc = function
    | From_dune_file loc -> Some loc
    | Internal
    | Source_file_copy ->
      None
end

module Promote = struct
  module Lifetime = struct
    type t =
      | Unlimited
      | Until_clean
  end

  module Into = struct
    type t =
      { loc : Loc.t
      ; dir : string
      }
  end

  type t =
    { lifetime : Lifetime.t
    ; into : Into.t option
    ; only : Predicate_lang.Glob.t option
    }
end

module Mode = struct
  type t =
    | Standard
    | Fallback
    | Promote of Promote.t
    | Ignore_source_files
end

module Id = Id.Make ()

module T = struct
  type t =
    { id : Id.t
    ; context : Context.t option
    ; env : Env.t option
    ; action : Action.t Build.With_targets.t
    ; mode : Mode.t
    ; locks : Path.t list
    ; info : Info.t
    ; dir : Path.Build.t
    }

  let compare a b = Id.compare a.id b.id

  let equal a b = Id.equal a.id b.id

  let hash t = Id.hash t.id

  let to_dyn t : Dyn.t =
    Record
      [ ("id", Id.to_dyn t.id)
      ; ("loc", Dyn.Encoder.option Loc.to_dyn (Info.loc t.info))
      ]
end

include T
module O = Comparable.Make (T)
module Set = O.Set

let make ?(sandbox = Sandbox_config.default) ?(mode = Mode.Standard) ~context
    ~env ?(locks = []) ?(info = Info.Internal) ~dir action =
  let open Build.With_targets.O in
  let action =
    Build.With_targets.memoize "Rule.make"
      (Build.with_no_targets (Build.dep (Dep.sandbox_config sandbox)) >>> action)
  in
  let targets = action.targets in
  let dir =
    match Path.Build.Set.choose targets with
    | None -> (
      match info with
      | From_dune_file loc ->
        User_error.raise ~loc [ Pp.text "Rule has no targets specified" ]
      | _ -> Code_error.raise "Build_interpret.Rule.make: no targets" [] )
    | Some _ ->
      ( if
      (* CR aalekseyev: do this properly *)
        false && Path.Build.Set.exists targets ~f:(fun path ->
            Path.Build.( <> ) (Path.Build.parent_exn path) dir)
      then
        match info with
        | Internal
        | Source_file_copy ->
          Code_error.raise "rule has targets in different directories"
            [ ("targets", Path.Build.Set.to_dyn targets) ]
        | From_dune_file loc ->
          User_error.raise ~loc
            [ Pp.text "Rule has targets in different directories.\nTargets:"
            ; Pp.enumerate (Path.Build.Set.to_list targets) ~f:(fun p ->
                  Pp.verbatim (Path.to_string_maybe_quoted (Path.build p)))
            ] );
      dir
  in
  { id = Id.gen (); context; env; action; mode; locks; info; dir }

let with_prefix t ~build =
  { t with
    action =
      (let open Build.With_targets.O in
      Build.With_targets.memoize "Rule.with_prefix"
        (Build.with_no_targets build >>> t.action))
  }

let loc t =
  match (t.info : Info.t) with
  | From_dune_file loc -> loc
  | Internal
  | Source_file_copy ->
    let dir = Path.drop_optional_build_context_src_exn (Path.build t.dir) in
    let file =
      match Option.bind (File_tree.find_dir dir) ~f:File_tree.Dir.dune_file with
      | Some file -> File_tree.Dune_file.path file
      | None -> Path.Source.relative dir "_unknown_"
    in
    Loc.in_file (Path.source file)

let effective_env t =
  match (t.env, t.context) with
  | None, None -> Env.initial
  | Some e, _ -> e
  | None, Some c -> c.env

let rule_deps t = (Build.static_deps t.action.build).rule_deps

let static_action_deps t = (Build.static_deps t.action.build).action_deps

(* CR-soon amokhov: Build [request] directly instead of going via a fake rule. *)
let shim_of_build_goal request =
  let request =
    let open Build.O in
    let+ () = request in
    Action.empty
  in
  { id = Id.gen ()
  ; context = None
  ; dir = Path.Build.root
  ; env = None
  ; action =
      Build.With_targets.memoize "Rule.shim_of_build_goal"
        (Build.with_no_targets request)
  ; mode = Mode.Standard
  ; locks = []
  ; info = Info.Internal
  }

let target_dirs (t : t) = 
  Path.Build.Set.map t.action.targets 
    ~f:(fun target -> Path.Build.parent_exn target)
