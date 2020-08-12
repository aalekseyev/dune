open Stdune

let temp_dir = lazy (Temp.create Dir ~prefix:"build" ~suffix:".dune")

let file ~prefix ~suffix =
  Temp.temp_in_dir File ~dir:(Lazy.force temp_dir) ~suffix ~prefix

let add_to_env env =
  let value = Path.to_absolute_filename (Lazy.force temp_dir) in
  match env with
  | None -> Env.add Env.initial ~var:Env.Var.temp_dir ~value
  | Some env ->
    Env.update env ~var:Env.Var.temp_dir ~f:(function
      | None -> Some value
      | Some _ as s -> s)

let destroy = Temp.destroy

let clear () = if Lazy.is_val temp_dir then Temp.clear (Lazy.force temp_dir)

let () = Hooks.End_of_build.always clear
