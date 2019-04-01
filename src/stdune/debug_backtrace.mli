
(* A backtrace that's only captured if dune is run with [--debug-extra-backtraces] *)
type t

val should_record : bool ref Fdecl.t

val create : unit -> t

val to_string : t -> string
