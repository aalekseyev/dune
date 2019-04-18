open! Stdune

val get_install_entries : File_binding.Expanded.t Dune_file.Install_conf.t list ->
  (Loc.t option * Install.Entry.t) list

val get_bin_install_entries :
  File_binding.Expanded.t Dune_file.Install_conf.t list -> context:Context.t -> Path.Set.t

val all_installs :
  Stanza.t list Dir_with_dune.t list ->
  expander:(dir:Path.t -> Expander.t) ->
  File_binding.Expanded.t Dune_file.Install_conf.t list
