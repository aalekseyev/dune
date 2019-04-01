let debug_findlib = ref false
let debug_dep_path = ref false
let external_lib_deps_hint = ref []
let capture_outputs = ref true

let debug_backtraces = ref false
let debug_extra_backtraces = ref false

let () =
  let open Stdune in
  Fdecl.set Debug_backtrace.should_record debug_extra_backtraces

let diff_command = ref None
let auto_promote = ref false
let force = ref false
let watch = ref false
let no_print_directory = ref false
let store_orig_src_dir = ref false
