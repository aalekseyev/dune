open Stdune

let dune_action_env_variable = "DUNE_ACTION_PIPES"

module Dependency = struct
  type t =
    | File of Path.t
    | Directory of Path.t
    | Universe
end

module File_descr = struct
  type t = Unix.file_descr

  (* external to_int : t -> int = "%identity" *)

  external of_int : int -> t = "%identity"

  let of_string string = of_int (Int.of_string_exn string)

  (* let to_string t = Int.to_string (to_int t) *)
end

let is_run_by_dune () =
  Sys.getenv_opt dune_action_env_variable |> Option.is_some

let serialize_deps _deps = failwith "unimplemented"

let wait_for_dependencies deps =
  let pipes = Sys.getenv dune_action_env_variable in
  let in_pipe, out_pipe =
    match Csexp.parse (Stream.of_string pipes) with
    | Ok (List [ Atom out_pipe; Atom in_pipe ]) -> (in_pipe, out_pipe)
    | _ -> failwith ""
  in
  let in_pipe, out_pipe =
    ( File_descr.of_string in_pipe |> Unix.in_channel_of_descr
    , File_descr.of_string out_pipe |> Unix.out_channel_of_descr )
  in
  let serialized_deps = serialize_deps deps in
  output_string out_pipe (String.concat ~sep:"" [ serialized_deps; "\n" ]);
  let response = input_line in_pipe in
  assert (response = "OK")
