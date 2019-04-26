open! Stdune

module T = struct

  type t = {
    here : bool;
    children : children;
  }
  and
    children = {
    default : bool;
    exceptions : t String.Map.t;
  }

  let is_universal t =
    String.Map.is_empty t.children.exceptions && t.here && t.children.default

  let is_empty t =
    String.Map.is_empty t.children.exceptions && not t.here && not t.children.default

  let is_trivial ~value t = match value with
    | false -> is_empty t
    | true -> is_universal t

  let trivial v =
    {
      here = v;
      children = {
        default = v;
        exceptions = String.Map.empty;
      };
    }

  let empty = trivial false
  let universal = trivial true

  let trivial v = match v with
    | false -> empty
    | true -> universal

  let match_ t =
    if is_empty t then `Empty
    else if is_universal t then `Universal
    else `Nontrivial t

end

module Children = struct

  type set = T.t

  (* invariant: none of the [exceptions] values are trivially [default] *)
  type t = T.children = {
    default : bool;
    exceptions : T.t String.Map.t;
  }

  let exceptions t = t.exceptions
  let default t = t.default

  let create ~default ~exceptions = {
    default;
    exceptions =
      String.Map.filter exceptions ~f:(fun v -> not (T.is_trivial ~value:default v));
  }

end

let rec union x y =
  match T.match_ x, T.match_ y with
  | `Empty, _ -> y
  | _, `Empty -> x
  | `Universal, _ | _, `Universal -> T.universal
  | `Nontrivial _, `Nontrivial _ ->
    { here = x.here || y.here;
      children = union_children x.children y.children
    }
and
  union_children x y =
  Children.create
    ~default:(x.default || y.default)
    ~exceptions:(
      String.Map.merge
        x.exceptions
        y.exceptions
        ~f:(fun _key vx vy ->
          let vx = Option.value vx ~default:(T.trivial x.default) in
          let vy = Option.value vy ~default:(T.trivial y.default) in
          Some (union vx vy)))

let rec intersect x y =
  match T.match_ x, T.match_ y with
  | `Universal, _ -> y
  | _, `Universal -> x
  | `Empty, _ | _, `Empty -> T.empty
  | `Nontrivial _, `Nontrivial _ ->
    { here = x.here && y.here;
      children = intersect_children x.children y.children;
    }
and
  intersect_children x y =
  Children.create
    ~default:(x.default && y.default)
    ~exceptions:(
      String.Map.merge
        x.exceptions
        y.exceptions
        ~f:(fun _key vx vy ->
          let vx = Option.value vx ~default:(T.trivial x.default) in
          let vy = Option.value vy ~default:(T.trivial y.default) in
          Some (intersect vx vy)))

let rec negate x =
  match T.match_ x with
  | `Universal -> T.empty
  | `Empty -> T.universal
  | `Nontrivial { here; children } ->
    {
      here = not here;
      children =
        Children.create
          ~default:(not children.default)
          ~exceptions:(String.Map.map children.exceptions ~f:(negate));
    }

include T

let rec mem t dir = match dir with
  | [] -> t.here
  | child :: rest ->
    match String.Map.find t.children.exceptions child with
    | None -> t.children.default
    | Some t ->
      mem t rest

let mem t dir = mem t (Path.explode_after_build_dir_exn dir)

let descend t child =
  match String.Map.find t.children.exceptions child with
  | None -> trivial t.children.default
  | Some t -> t

let union_all = List.fold_left ~init:empty ~f:union

let of_subtree_gen subtree =
  let rec loop = function
    | [] -> subtree
    | component :: rest ->
      { here = false;
        children =
          Children.create ~default:false
            ~exceptions:(String.Map.singleton component (loop rest));
      }
  in
  fun path -> loop (Path.explode_after_build_dir_exn path)

let just_the_root = {
  here = true;
  children = Children.create ~default:false ~exceptions:String.Map.empty;
}

let of_subtrees paths =
  List.map paths ~f:(of_subtree_gen T.universal)
  |> union_all

let of_individual_dirs paths =
  List.map paths ~f:(of_subtree_gen just_the_root)
  |> union_all

type element =
  | One_dir of Path.t
  | Subtree of Path.t

let of_list list =
  List.map list ~f:(function
    | One_dir dir -> of_subtree_gen just_the_root dir
    | Subtree dir -> of_subtree_gen T.universal dir)
  |> union_all

let minus x y =
  (intersect x (negate y))

let is_subset x ~of_ =
  is_empty (minus x of_)

let rec to_sexp t = match match_ t with
  | `Empty -> Sexp.Atom "Empty"
  | `Universal -> Sexp.Atom "Universal"
  | `Nontrivial _ ->
    Sexp.List (
      (
        (match t.here with | true -> [ ".", Sexp.Atom "true" ] | false -> []) @
        (String.Map.to_list t.children.exceptions
         |> List.map ~f:(fun (s, t) ->
           s, to_sexp t)) @
        (match t.children.default with
         | false -> []
         | true -> [("*", Sexp.Atom "Universal")]))
      |> List.map ~f:(fun (k, v) -> Sexp.List [Sexp.Atom k; v]))
