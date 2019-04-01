type t = Printexc.raw_backtrace option

let should_record = Fdecl.create ()

let create () =
  if !(Fdecl.get should_record) then
    Some (Printexc.get_callstack 50)
  else
    None

let to_string t =
  match t with
  | None -> "<backtrace not recorded>"
  | Some t -> Printexc.raw_backtrace_to_string t
