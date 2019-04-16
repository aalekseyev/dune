open! Stdune

(** Represents a (potentially infinite) set of directories. Not any set can be specified,
    only ones that can be efficiently inspected. *)

type children

type t = {
  here : bool;
  children : children;
}

(* Total mapping from the child basename to a [t].
   Only a finite number of bindings can be non-trivial.

   The "trivial" ones will be either all [trivial true] or all [trivial false]. *)
module Children : sig
  type set = t
  type t = children

  val default : t -> bool
  val exceptions : t -> set String.Map.t

  val create : default:bool -> exceptions:set String.Map.t -> t
end

val empty : t
val universal : t

val mem : t -> Path.t -> bool

val descend : t -> string -> t

(** paths must be in build directory *)

val of_subtrees : Path.t list -> t
val of_individual_dirs : Path.t list -> t

type element =
  | One_dir of Path.t
  | Subtree of Path.t

val of_list : element list -> t

val is_subset : t -> of_:t -> bool

val union : t -> t -> t
val intersect : t -> t -> t
val negate : t -> t

val to_sexp : t -> Sexp.t
