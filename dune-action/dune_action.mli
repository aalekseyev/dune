open Stdune
module Protocol = Protocol

type 'a t

(* TODO jstaron: Add documentation. *)
(* TODO jstaron: What types should we use for paths?. *)

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val stage : 'a t -> ('a -> 'b t) -> 'b t

val read_file : path:Path.t -> (string, Unix.error) Result.t t

val write_file : path:Path.t -> data:string -> (unit, Unix.error) Result.t t

(* TODO jstaron: What should be the return type of [read_directory]? *)
val read_directory : path:Path.t -> (string list, Unix.error) Result.t t

val run : unit t -> unit

(* val stage : 'a -> f:('a -> 'b t) -> 'b t *)
