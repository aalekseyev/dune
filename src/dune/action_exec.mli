open! Stdune

type done_or_more_deps =
  | Done of Dep.Set.t (* Dynamic deps used by this exec call. *)
  | Need_more_deps of Dep.Set.t

val exec :
     targets:Path.Build.Set.t
  -> context:Context.t option
  -> env:Env.t option
  -> Action.t
  -> done_or_more_deps Fiber.t
