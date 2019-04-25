open! Stdune

let get_installed_binaries stanzas ~(context : Context.t) ~expander =
  let install_dir = Config.local_install_bin_dir ~context:context.name in
  Dir_with_dune.deep_fold stanzas ~init:Path.Set.empty ~f:(fun d stanza acc ->
    match (stanza : Stanza.t) with
    | Dune_file.Install { section = Bin; files; _ } ->
      let expand_str = Expander.expand_str (expander ~dir:d.ctx_dir) in
      List.fold_left files ~init:acc ~f:(fun acc fb ->
        match
          match File_binding.Unexpanded.expand_dst fb ~f:expand_str with
          | None ->
            Some
              (File_binding.Unexpanded.expand_src fb ~dir:d.ctx_dir
                 ~f:expand_str
               |> Path.basename)
          | Some p ->
            if Path.Local.is_root (Path.Local.parent_exn p) then
              Some (Path.Local.basename p)
            else
              None
        with
        | None -> acc
        | Some basename ->
          Path.Set.add acc (Path.relative install_dir basename))
    | _ -> acc)
