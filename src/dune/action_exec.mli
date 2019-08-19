open! Stdune

type done_or_more_deps =
  | Done
  | Need_more_deps of Dep.Set.t

val exec :
     targets:Path.Build.Set.t
  -> context:Context.t option
  -> env:Env.t option
  -> Action.t
  -> done_or_more_deps Fiber.t
