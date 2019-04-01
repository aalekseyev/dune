There are some install rules, and there is also some globbing.

This can create a dependency cycle if "install rules" try to
produce targets in the directory being globbed.

(install rules depend on library artifacts, which in turn depend
on the directory listing)

  $ dune build @install
  $ cat _build/default/bar/bar.install
  lib: [
    "_build/install/default/lib/bar/META" {"META"}
    "_build/install/default/lib/bar/bar$ext_lib" {"bar$ext_lib"}
    "_build/install/default/lib/bar/bar.cma" {"bar.cma"}
    "_build/install/default/lib/bar/bar.cmi" {"bar.cmi"}
    "_build/install/default/lib/bar/bar.cmt" {"bar.cmt"}
    "_build/install/default/lib/bar/bar.cmx" {"bar.cmx"}
    "_build/install/default/lib/bar/bar.cmxa" {"bar.cmxa"}
    "_build/install/default/lib/bar/bar.cmxs" {"bar.cmxs"}
    "_build/install/default/lib/bar/bar.ml" {"bar.ml"}
    "_build/install/default/lib/bar/bar__.cmi" {"bar__.cmi"}
    "_build/install/default/lib/bar/bar__.cmt" {"bar__.cmt"}
    "_build/install/default/lib/bar/bar__.cmx" {"bar__.cmx"}
    "_build/install/default/lib/bar/bar__.ml" {"bar__.ml"}
    "_build/install/default/lib/bar/bar__Baz.cmi" {"bar__Baz.cmi"}
    "_build/install/default/lib/bar/bar__Baz.cmt" {"bar__Baz.cmt"}
    "_build/install/default/lib/bar/bar__Baz.cmx" {"bar__Baz.cmx"}
    "_build/install/default/lib/bar/baz.ml" {"baz.ml"}
    "_build/install/default/lib/bar/dune-package" {"dune-package"}
    "_build/install/default/lib/bar/opam" {"opam"}
  ]
