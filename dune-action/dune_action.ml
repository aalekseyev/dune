open Stdune
open Protocol

let ( >>> ) f g x = g (f x)

module Stage = struct
  type 'a t =
    { action : unit -> 'a (* TODO jstaron: Replace with Dep.t *)
    ; dependencies : Dependency.t list
    }

  let return a = { action = (fun () -> a); dependencies = [] }

  let map (t : 'a t) ~f = { t with action = t.action >>> f }

  let both (t1 : 'a t) (t2 : 'b t) =
    { action =
      (fun () -> (t1.action (), t2.action ()))
      (* TODO jstaron: Deduplication. *)
    ; dependencies = List.concat [ t1.dependencies; t2.dependencies ]
    }
end

include Stage

let stage (_t : 'a t) = failwith "unimplemented"

let read_file ~path =
  (* TODO jstaron: Catch Unix.error exceptions. *)
  let action () = Io.read_file path |> Result.ok in
  { action; dependencies = [ File path ] }

let write_file ~path ~data =
  (* TODO jstaron: Catch Unix.error exceptions. *)
  let action () = Io.write_file path data |> Result.ok in
  { action; dependencies = [] }

let read_directory ~path =
  let action () =
    Path.readdir_unsorted path
    |> Result.map ~f:(List.sort ~compare:String.compare)
  in
  { action; dependencies = [ Directory path ] }

let run t =
  if not (Protocol.is_run_by_dune ()) then
    failwith "This executable must be run by dune.";
  Protocol.wait_for_dependencies t.dependencies;
  t.action ()
