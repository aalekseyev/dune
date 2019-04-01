open! Stdune

module Path = struct
  type t = Path.t

  module Set = Path.Set
  module Map = Path.Map
  let explode = Path.explode_after_build_dir_exn
  let is_descendant = Path.is_descendant
end

module T = struct
  type 'rules t =
    | Empty
    | Union of 'rules t * 'rules t
    | Approximation_subtree of Path.Set.t * 'rules t
    | Finite of 'rules Path.Map.t
    | Thunk of (unit -> 'rules t)
end

include T

module Evaluated = struct
  type 'rules t = {
    by_child : 'rules t Memo.Lazy.t String.Map.t;
    rules_here : 'rules Memo.Lazy.t;
  }
end

(** Represents a set of subtrees that could potentially contain rules.

    The set is encoded as a trie with [Here] constructors representing
    the elements of the set.
*)
module Subtree_set = struct

  type t =
    | Here
    | Descend of t String.Map.t

  let empty = Descend String.Map.empty

  let rec merge x y =
    match (x, y) with
    | Here, _ | _, Here -> Here
    | Descend m1, Descend m2 ->
      Descend (String.Map.union m1 m2 ~f:(fun _ x y -> Some (merge x y)))

  let merge_all = List.fold_left ~init:empty ~f:merge

  let of_path =
    let rec loop = function
      | [] -> Here
      | component :: rest ->
        Descend (String.Map.singleton component (loop rest))
    in
    fun path -> loop (Path.explode path)

  let of_paths paths =
    List.map paths ~f:of_path
    |> merge_all

  exception Not_a_subset

  let is_subset x ~of_ =
    let rec go x y =
      match x, y with
      | _, Here -> ()
      | Here, (Descend _) -> raise Not_a_subset
      | Descend m1, Descend m2 ->
        let _ : Nothing.t String.Map.t = String.Map.merge m1 m2 ~f:(fun _key v1 v2 ->
          let v1 = Option.value ~default:empty v1 in
          let v2 = Option.value ~default:empty v2 in
          go v1 v2;
          None)
        in
        ()
    in
    match go x of_ with
    | exception Not_a_subset -> false
    | () -> true

end

module Make(Rules : sig
    type t
    val empty : t
    val union : t -> t -> t
  end) = struct

  type t = Rules.t T.t

  module Evaluated = struct
    type 'rules t_gen = 'rules Evaluated.t =  {
      by_child : 'rules t_gen Memo.Lazy.t String.Map.t;
      rules_here : 'rules Memo.Lazy.t;
    }
    type t = Rules.t t_gen

    let empty =
      { by_child = String.Map.empty;
        rules_here = Memo.Lazy.of_val Rules.empty;
      }

    let descend t dir =
      match String.Map.find t.by_child dir with
      | None -> empty
      | Some res -> Memo.Lazy.force res

    let rec union ~union_rules x y =
      {
        by_child =
          String.Map.union x.by_child y.by_child
            ~f:(fun _key data1 data2 ->
              Some (Memo.Lazy.map2 data1 data2 ~f:(fun x y -> union ~union_rules x y)));
        rules_here =
          Memo.Lazy.map2 x.rules_here y.rules_here ~f:union_rules
      }

    let union = union ~union_rules:Rules.union

    let rec restrict (dirs : Subtree_set.t) t : t = match dirs with
      | Here -> Memo.Lazy.force t
      | Descend by_child ->
        {
          by_child = String.Map.mapi by_child ~f:(fun dir restriction ->
            Memo.lazy_ (fun () ->
              restrict restriction
                (Memo.lazy_ (fun () -> descend (Memo.Lazy.force t) dir)))
          );
          rules_here = Memo.Lazy.of_val Rules.empty;
        }

    let singleton path (rules : Rules.t) =
      let rec go = function
        | [] ->
          { by_child = String.Map.empty; rules_here = Memo.Lazy.of_val rules; }
        | x :: xs ->
          {
            by_child = String.Map.singleton x (Memo.Lazy.of_val (go xs));
            rules_here = Memo.Lazy.of_val Rules.empty;
          }
      in
      go (Path.explode path)

    let finite m =
      Path.Map.to_list m
      |> List.map ~f:(fun (path, rules) ->
        singleton path rules)
      |> List.fold_left ~init:empty ~f:union

  end

  let rec evaluate ~env = function
    | Empty -> Evaluated.empty
    | Union (x, y) -> Evaluated.union (evaluate ~env x) (evaluate ~env y)
    | Approximation_subtree (paths, rules) ->
      let paths =
        Subtree_set.of_paths (Path.Set.to_list paths)
      in
      if not (Subtree_set.is_subset paths ~of_:env)
      then
        raise (Exn.code_error
          "inner [Approximate] specifies a wider set than the outer [Approximate]"
          [])
      else
        Evaluated.restrict paths (Memo.lazy_ (fun () -> evaluate ~env:paths rules))
    | Finite rules -> Evaluated.finite rules
    | Thunk f -> evaluate ~env (f ())

  let all l = List.fold_left ~init:T.Empty ~f:(fun x y -> T.Union (x, y)) l

  module For_tests = struct
    (* [collect_rules_simple] is oversimplified in two ways:
       - it does not share the work of scheme flattening, so repeated lookups do
       repeated work
       - it does not check that approximations are correct

       If approximations are not correct, it will honor the approximation.
       So approximations act like views that prevent the rules from being seen
       rather than from being declared in the first place.
    *)
    let collect_rules_simple =
      let rec go (t : t) ~dir =
        match t with
        | Empty -> Rules.empty
        | Union (a, b) -> Rules.union(go a ~dir) (go b ~dir)
        | Approximation_subtree (dirs, t) ->
          (match Path.Set.exists dirs ~f:(fun p -> Path.is_descendant ~of_:p dir) with
           | true -> go t ~dir
           | false -> Rules.empty)
        | Finite rules ->
          (match Path.Map.find rules dir with
           | None -> Rules.empty
           | Some rule -> rule)
        | Thunk f ->
          go (f ()) ~dir
      in
      go

  end

  let get_rules : Evaluated.t -> dir:Path.t -> Rules.t =
    fun t ~dir ->
      let dir = Path.explode dir in
      let t = List.fold_left dir ~init:t ~f:Evaluated.descend in
      Memo.Lazy.force t.rules_here

  let evaluate = evaluate ~env:Subtree_set.empty

end

module For_tests = struct

  let instrument ~print =
    let print path suffix =
      print (String.concat (List.rev path @ [suffix]) ~sep:":")
    in
    let rec go ~path t = match t with
      | Empty -> Empty
      | Union (t1, t2) ->
        Union (go ~path:("l"::path) t1, go ~path:("r"::path) t2)
      | Approximation_subtree (dirs, rules) ->
        let path = "t" :: path in
        Approximation_subtree (dirs, go ~path rules)
      | Finite m -> Finite m
      | Thunk t ->
        Thunk (fun () ->
          print path "thunk";
          t ())
    in
    go ~path:[]
end
