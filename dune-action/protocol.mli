open Stdune

module Dependency : sig
  type t =
    | File of Path.t
    | Directory of Path.t
    | Universe
end

val is_run_by_dune : unit -> bool

val wait_for_dependencies : Dependency.t list -> unit
