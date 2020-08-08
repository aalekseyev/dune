(** Temporary file management *)

(** This module provides a high-level interface for temporary files. It ensures
    that all temporary files created by the application are systematically
    cleaned up on exit. *)

type what =
  | Dir
  | File

val temp_in_dir : what -> dir:Path.t -> prefix:string -> suffix:string -> Path.t

val create : what -> prefix:string -> suffix:string -> Path.t

val destroy : what -> Path.t -> unit

val clear : Path.t -> unit
