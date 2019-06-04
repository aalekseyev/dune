open Stdune
open Dune_file

module T = struct
  type is_component_of_a_group_but_not_the_root = {
    group_root : Path.Build.t;
    stanzas : Stanza.t list Dir_with_dune.t option;
  }

  type t =
    | Standalone of
        (File_tree.Dir.t * Stanza.t list Dir_with_dune.t option) option
    (* Directory not part of a multi-directory group. The argument is
       [None] for directory that are not from the source tree, such as
       generated ones. *)

    | Group_root of File_tree.Dir.t
                    * Include_subdirs.qualification
                    * Stanza.t list Dir_with_dune.t
    (* Directory with [(include_subdirs x)] where [x] is not [no] *)

    | Is_component_of_a_group_but_not_the_root of is_component_of_a_group_but_not_the_root
    (* Sub-directory of a [Group_root _] *)

  let to_sexp _ = Sexp.Atom "<dir-status is opaque>"
end
include T

type enclosing_group =
  | No_group
  | Group_root of Path.Build.t

let current_group dir = function
  | Standalone _ -> No_group
  | Group_root _ -> Group_root dir
  | Is_component_of_a_group_but_not_the_root { group_root; _ } ->
    Group_root group_root

let get_include_subdirs stanzas =
  List.fold_left stanzas ~init:None ~f:(fun acc stanza ->
    match stanza with
    | Include_subdirs (loc, x) ->
      if Option.is_some acc then
        Errors.fail loc "The 'include_subdirs' stanza cannot appear \
                         more than once";
      Some x
    | _ -> acc)

let check_no_module_consumer stanzas =
  List.iter stanzas ~f:(fun stanza ->
    match stanza with
    | Library { buildable; _} | Executables { buildable; _ }
    | Tests { exes = { buildable; _ }; _ } ->
      Errors.fail buildable.loc
        "This stanza is not allowed in a sub-directory of directory with \
         (include_subdirs unqualified).\n\
         Hint: add (include_subdirs no) to this file."
    | _ -> ())

module DB = struct

  type nonrec t =
    { file_tree : File_tree.t
    ; stanzas_per_dir
      : Dune_file.Stanzas.t Dir_with_dune.t Path.Build.Map.t
    ; fn : (Path.Build.t, t) Memo.Sync.t
    }

  let stanzas_in db ~dir =
    Path.Build.Map.find db.stanzas_per_dir dir

  let get db ~dir =
    let get ~dir = Memo.exec db.fn dir in
    let enclosing_group ~dir =
      match Path.Build.parent dir with
      | None ->
        No_group
      | Some parent_dir ->
        current_group parent_dir (get ~dir:parent_dir)
    in
    match
      Option.bind (Path.Build.drop_build_context dir)
        ~f:(File_tree.find_dir db.file_tree)
    with
    | None -> begin
        match enclosing_group ~dir  with
        | No_group -> Standalone None
        | Group_root group_root ->
          Is_component_of_a_group_but_not_the_root
            { stanzas = None; group_root }
      end
    | Some ft_dir ->
      let project_root =
        File_tree.Dir.project ft_dir
        |> Dune_project.root
      in
      let build_dir_is_project_root =
        Path.Build.drop_build_context_exn dir
        |> Path.Source.equal project_root
      in
      match stanzas_in db ~dir with
      | None ->
        if build_dir_is_project_root then
          Standalone (Some (ft_dir, None))
        else
          (match enclosing_group ~dir with
           | No_group ->
             Standalone (Some (ft_dir, None))
           | Group_root group_root ->
             Is_component_of_a_group_but_not_the_root
               { stanzas = None; group_root })
      | Some d ->
        match get_include_subdirs d.data with
        | Some (Include mode) ->
          Group_root (ft_dir, mode, d)
        | Some No ->
          Standalone (Some (ft_dir, Some d))
        | None ->
          if build_dir_is_project_root then
            Standalone (Some (ft_dir, Some d))
          else begin
            match enclosing_group ~dir with
            | Group_root group_root ->
              (
                check_no_module_consumer d.data;
                Is_component_of_a_group_but_not_the_root
                  { stanzas = (Some d); group_root })
            | No_group ->
              Standalone (Some (ft_dir, Some d))
          end

  let make file_tree ~stanzas_per_dir =
    let fn, set_fn_impl =
      Memo.create_fdecl
        "get-dir-status"
        ~input:(module Path.Build)
        ~visibility:Hidden
        ~output:(Simple (module T))
        ~doc:"Get a directory status."
        Sync
    in
    let t =
      { file_tree
      ; stanzas_per_dir
      ; fn
      }
    in
    set_fn_impl (fun dir -> get t ~dir);
    t

  let get db ~dir =
    Memo.exec db.fn dir
end
