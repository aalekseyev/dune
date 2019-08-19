open Stdune

module Dependency : sig
  type t =
    | File of Path.t
    | Directory of Path.t
    | Universe
end

val dune_action_env_variable : string

val is_run_by_dune : unit -> bool

val wait_for_dependencies : Dependency.t list -> unit
