open! Stdune

type rule = unit -> unit

module T = struct
  type t = rule Path.Map.t

  let empty = Path.Map.empty

  let union_map a b ~f = Path.Map.union a b ~f:(fun _key a b -> Some (f a b))

  let union =
    union_map ~f:(fun rule1 rule2 -> fun () -> rule1 (); rule2 ())

  let name = "Rules"
end

include T

let file_rule ~rule:(dst, rule) =
  Path.Map.singleton (Path.parent_exn dst) rule

let dir_rule (dir, rule) =
  Path.Map.singleton dir rule

let implicit_output = Memo.Implicit_output.add(module T)

let file_rule ~rule =
  Memo.Implicit_output.produce implicit_output (file_rule ~rule)

let dir_rule arg =
  Memo.Implicit_output.produce implicit_output (dir_rule arg)

let collect f =
  let result, out = Memo.Implicit_output.collect_sync implicit_output f in
  result, Option.value out ~default:T.empty

let to_map x = x
