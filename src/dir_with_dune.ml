open Stdune

type 'data t =
  { src_dir         : Path.t
  ; ctx_dir         : Path.t
  ; data            : 'data
  ; scope           : Scope.t
  ; kind            : Dune_lang.Syntax.t
  ; dune_version    : Syntax.Version.t
  }

let data t = t.data

let map t ~f = { t with data = f t.data }
