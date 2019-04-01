(** [Rules] represents a collection of rules across a known set of directories. *)

open! Stdune

(** [rule] is a function that produces some build system rules
    such as ([Build_system.add_rule]) in a known directory. *)
type rule = unit -> unit

type t = private rule Path.Map.t

val file_rule : rule:(Path.t * rule) -> unit

val dir_rule : (Path.t * rule) -> unit

val union : t -> t -> t

val collect : (unit -> 'a) -> ('a * t)
