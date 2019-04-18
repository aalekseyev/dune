open! Stdune
open! Import

module Bin : sig
  type t

  (** A named artifact that is looked up in the PATH if not found in the tree
      If the name is an absolute path, it is used as it.
  *)
  val binary
    :  t
    -> ?hint:string
    -> loc:Loc.t option
    -> string
    -> Action.Prog.t

  val add_binaries
    :  t
    -> dir:Path.t
    -> File_binding.Expanded.t list
    -> t

  module Partial : sig
    type t

    val empty : t

    val merge : shadowing:t -> shadowed:t -> t

    val add_binaries
      :  t
      -> dir:Path.t
      -> File_binding.Expanded.t list
      -> t
  end

  val create' : Context.t -> Partial.t -> t

end

module Public_libs : sig
  type t

  (** [file_of_lib t ~from ~lib ~file] returns the path to a file in the
      directory of the given library. *)
  val file_of_lib
    :  t
    -> loc:Loc.t
    -> lib:Lib_name.t
    -> file:string
    -> (Path.t, fail) result
end

type t = {
  public_libs : Public_libs.t;
  bin : Bin.t;
}

val create
  :  Context.t
  -> public_libs:Lib.DB.t
  -> t
