open! Stdune

(** [Scheme] is a collection of rules for one or multiple directories. *)

type 'rules t =
  | Empty
  | Union of 'rules t * 'rules t
  | Approximation_subtree of Path.Set.t * 'rules t
  | Finite of 'rules Path.Map.t
  | Thunk of (unit -> 'rules t)

module Make(Directory_rules : sig
    type t
    val empty : t
    val union : t -> t -> t
    val concat : t list -> t
    val thunk : (unit -> t) -> t
  end) : sig

  type nonrec t = Directory_rules.t t

  module Evaluated : sig
    type t
  end

  module For_tests : sig
    val collect_rules_simple : t -> dir:Path.t -> Directory_rules.t
  end

  val evaluate : t -> Evaluated.t

  val get_rules : Evaluated.t -> dir:Path.t -> Directory_rules.t

  val all : t list -> t
end

module For_tests : sig
  (* calls [print] every time any code embedded in the [Scheme] runs, be it
     a [Thunk] constructor or an [Approximation] function.

     The argument of [print] identifies which thunk got run (the path to that thunk
     within the [Scheme.t] value).
  *)
  val instrument : print:(string -> unit) -> 'a t -> 'a t
end
