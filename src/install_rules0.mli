open! Stdune

val get_install_entries : File_binding.Expanded.t Dune_file.Install_conf.t list ->
  (Loc.t option * Install.Entry.t) list

val get_bin_install_entries :
  Stanza.t list Dir_with_dune.t list
  -> context:Context.t
  -> expander:(dir:Path.t -> Expander.t)
  -> Path.Set.t
