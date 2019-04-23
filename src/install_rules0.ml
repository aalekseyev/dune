open! Stdune

let get_install_entries installs =
  List.concat_map installs
    ~f:(fun { Dune_file.Install_conf. section; files; package = _ } ->
      List.map files ~f:(fun fb ->
        let loc = File_binding.Expanded.src_loc fb in
        let src = File_binding.Expanded.src fb in
        let dst = Option.map ~f:Path.Local.to_string
                    (File_binding.Expanded.dst fb) in
        ( Some loc
        , Install.Entry.make section src ?dst
        )))

(* The code below ([all_installs], [expand_stanza], [get_bin_install_entries] is largely
   duplicating the logic of install rules set up. This is done to avoid dependency
   cycles. (to learn the set of available binaries you need to compute all install
   rules, which make use of something that depends on those binaries) *)
let get_bin_install_entries installs ~context =
  List.concat_map installs
    ~f:(fun { Dune_file.Install_conf. section; files; package = _ } ->
      match section with
      | Bin ->
        List.map files ~f:(fun fb ->
          let src = File_binding.Expanded.src fb in
          let dst = Option.map ~f:Path.Local.to_string
                      (File_binding.Expanded.dst fb) in
          let install_dir = Config.local_install_dir ~context:context.Context.name in
          Path.append install_dir (Install.Entry.relative_installed_path_for_bin
                                     (Install.Entry.make section src ?dst))
        )
      | _ -> []
    )
  |> Path.Set.of_list

let expand_stanza ~expander ~dir i =
  let expander = expander ~dir in
  let path_expander =
    File_binding.Unexpanded.expand ~dir
      ~f:(Expander.expand_str expander)
  in
  let open Dune_file in
  let files = List.map ~f:path_expander i.Install_conf.files in
  { i with files }

let all_installs stanzas ~expander =
  List.concat_map stanzas
    ~f:(fun ({ Dir_with_dune. data = stanzas; ctx_dir = dir; _ }) ->
      List.filter_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Dune_file.Install install ->
          Some (expand_stanza ~expander ~dir install)
        | _ -> None))

let get_bin_install_entries stanzas ~context ~expander =
  get_bin_install_entries
    ~context
    (all_installs ~expander stanzas)
