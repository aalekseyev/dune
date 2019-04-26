open! Stdune

open Scheme_intf

module Path = struct
  type t = Path.t

  let to_string = Path.to_string
  module Set = Path.Set
  module Map = Path.Map
  let explode = Path.explode_after_build_dir_exn
  let of_parts parts = match parts with
    | [] -> Path.build_dir
    | _ ->
      Path.relative Path.build_dir (String.concat ~sep:"/" parts)
end

module Gen' = struct
  include Gen
  module Evaluated = struct
    type 'rules t = {
      children : 'rules children Memo.Lazy.t;
      rules_here : 'rules Memo.Lazy.t;
    }
    and
      'rules children = {
      by_child : 'rules t String.Map.t;
      on_demand : (string, 'rules t, (string -> 'rules t)) Memo.t option;
    }
  end
end

module Make(Rules : sig
    type t
    val empty : t
    val union : t -> t -> t
  end) = struct

  type 'rules t_gen = 'rules Gen.t =
    | Empty
    | Union of 'rules t_gen * 'rules t_gen
    | Approximation of Dir_set.t * 'rules t_gen
    | Finite of 'rules Path.Map.t
    | Thunk of (unit -> 'rules t_gen)
    | By_dir of (dir:Path.t -> 'rules)

  type t = Rules.t Gen.t

  module Evaluated = struct
    type 'rules t_gen = 'rules Gen'.Evaluated.t = {
      children : 'rules children Memo.Lazy.t;
      rules_here : 'rules Memo.Lazy.t;
    }
    and
      'rules children = 'rules Gen'.Evaluated.children = {
      by_child : 'rules t_gen String.Map.t;
      on_demand : (string, 'rules t_gen, (string -> 'rules t_gen)) Memo.t option;
    }

    type t = Rules.t t_gen

    let no_children =
      Memo.Lazy.of_val {
        by_child = String.Map.empty;
        on_demand = None;
      }

    let empty =
      { rules_here = Memo.Lazy.of_val Rules.empty;
        children = no_children
      }

    let string_memo f =
      Memo.create_opaque "string-memo" ~input:(module String) Sync f

    let of_function =
      let rec go f acc =
        {
          rules_here = Memo.lazy_ (fun () -> f (List.rev acc));
          children = Memo.Lazy.of_val {
            by_child = String.Map.empty;
            on_demand = Some (string_memo (fun s -> go f (s :: acc)));
          }
        }
      in
      fun f -> go f []

    let rec union ~union_rules x y =
      {
        rules_here =
          Memo.Lazy.map2 x.rules_here y.rules_here ~f:union_rules;
        children =
          Memo.lazy_ (fun () ->
            let x = Memo.Lazy.force x.children in
            let y = Memo.Lazy.force y.children in
            { by_child =
                String.Map.union x.by_child y.by_child
                  ~f:(fun _key x y ->
                    Some (union ~union_rules x y));
              on_demand = (match x.on_demand, y.on_demand with
                | None, x | x, None -> x
                | Some x, Some y ->
                  Some (string_memo (fun s ->
                    union ~union_rules
                      (Memo.exec x s)
                      (Memo.exec y s)
                  )));
            });
      }

    let union = union ~union_rules:Rules.union

    let descend' children dir =
      let scheme1 =
        match String.Map.find children.by_child dir with
        | None -> empty
        | Some res -> res
      in
      let scheme2 =
        match children.on_demand with
        | None -> empty
        | Some on_demand ->
          Memo.exec on_demand dir
      in
      union scheme1 scheme2

    let descend t dir =
      let children = Memo.Lazy.force t.children in
      descend' children dir
    ;;

    let of_lazy l =
      { rules_here = Memo.Lazy.bind l ~f:(fun l -> l.rules_here);
        children = Memo.Lazy.bind l ~f:(fun l -> l.children);
      }

    let lazy_ f = of_lazy (Memo.lazy_ f)

    let rec restrict (dirs : Dir_set.t) t : _ t_gen =
      {
        rules_here =
          (if dirs.here then
             t.rules_here
           else
             Memo.Lazy.of_val Rules.empty);
        children = (
          if Dir_set.is_empty (Dir_set.minus dirs Dir_set.just_the_root)
          then
            no_children
          else
            match Dir_set.Children.default dirs.children with
            | true ->
              Memo.Lazy.map t.children ~f:(fun children ->
                (* This is forcing [t.children] potentially too early if the directory
                   the user is interested in is not actually in the set [dirs].
                   We're not particularly committed to supporting exceptions in that case
                   though. *)
                { by_child =
                    String.Map.mapi children.by_child
                      ~f:(fun dir v ->
                        restrict
                          (Dir_set.descend dirs dir)
                          v);
                  on_demand =
                    match children.on_demand with
                    | None -> None
                    | Some on_demand ->
                      Some (string_memo (fun dir ->
                        restrict
                          (Dir_set.descend dirs dir)
                          (lazy_ (fun () ->
                             Memo.exec on_demand dir
                           ))
                      ))
                })
            | false ->
              Memo.Lazy.of_val
                { on_demand = None;
                  by_child =
                    String.Map.mapi (Dir_set.Children.exceptions dirs.children)
                      ~f:(fun dir v ->
                        restrict
                          v
                          (lazy_ (fun () -> descend' (Memo.Lazy.force t.children) dir)));
                })
      }

    let singleton path (rules : Rules.t) =
      let rec go = function
        | [] ->
          { children = no_children; rules_here = Memo.Lazy.of_val rules; }
        | x :: xs ->
          {
            children = Memo.Lazy.of_val {
              by_child = String.Map.singleton x (go xs);
              on_demand = None;
            };
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
    | Approximation (paths, rules) ->
      if
        not (Dir_set.is_subset paths ~of_:env)
        && not (Dir_set.is_subset (Dir_set.negate paths) ~of_:env)
      then
        raise (Exn.code_error
                 "inner [Approximate] specifies a set such that neither it, nor its \
                  negation, are a subset of directories specified by the outer \
                  [Approximate]."
                 [
                   "inner", (Dir_set.to_sexp paths);
                   "outer", (Dir_set.to_sexp env);
                 ])
      else
        let paths = Dir_set.intersect paths env in
        Evaluated.restrict paths
          (Evaluated.of_lazy (Memo.lazy_ (fun () -> evaluate ~env:paths rules)))
    | Finite rules -> Evaluated.finite rules
    | Thunk f -> evaluate ~env (f ())
    | By_dir f ->
      Evaluated.of_function (fun dir -> f ~dir:(Path.of_parts dir))

  let all l = List.fold_left ~init:Empty ~f:(fun x y -> Union (x, y)) l

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
        | Approximation (dirs, t) ->
          (match Dir_set.mem dirs dir with
           | true -> go t ~dir
           | false -> Rules.empty)
        | Finite rules ->
          (match Path.Map.find rules dir with
           | None -> Rules.empty
           | Some rule -> rule)
        | Thunk f ->
          go (f ()) ~dir
        | By_dir f ->
          f ~dir
      in
      go

  end

  let get_rules : Evaluated.t -> dir:Path.t -> Rules.t =
    fun t ~dir ->
      let dir = Path.explode dir in
      let t = List.fold_left dir ~init:t ~f:Evaluated.descend in
      Memo.Lazy.force t.rules_here

  let evaluate = evaluate ~env:Dir_set.universal

end

module Rules_scheme = Make(struct
    type t = unit -> unit
    let empty = (fun () -> ())
    let union f g () = f (); g ()
  end)

include Rules_scheme

module Gen = struct
  module For_tests = struct

    let instrument ~print =
      let print path suffix =
        print (String.concat (List.rev path @ [suffix]) ~sep:":")
      in
      let rec go ~path t = match t with
        | Gen.Empty -> Gen.Empty
        | Union (t1, t2) ->
          Union (go ~path:("l"::path) t1, go ~path:("r"::path) t2)
        | Approximation (dirs, rules) ->
          let path = "t" :: path in
          Approximation (dirs, go ~path rules)
        | Finite m -> Finite m
        | Thunk t ->
          Thunk (fun () ->
            print path "thunk";
            t ())
        | By_dir f ->
          By_dir (fun ~dir ->
            print path (Printf.sprintf "by-dir:%s" (Path.to_string dir));
            f ~dir
          )
      in
      go ~path:[]
  end
end
