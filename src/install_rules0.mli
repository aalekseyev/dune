open! Stdune

val get_bin_install_entries :
  Stanza.t list Dir_with_dune.t list
  -> context:Context.t
  -> expander:(dir:Path.t -> Expander.no_env Expander.t_gen)
  -> Path.Set.t
