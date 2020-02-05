
  $ cat >dune-project <<EOF
  > (lang dune 2.2)
  > EOF
  $ cat >dune <<EOF
  > (has_rules_for_subdirs true)
  > (rule (targets x foo/y) (action (progn (with-stdout-to x (echo hello)) (with-stdout-to foo/y (echo world)))))
  > EOF

  $ dune build _build/default/foo/y
  $ cat _build/default/x
  hello
  $ cat _build/default/foo/y
  world

  $ rm -r _build

  $ dune build _build/default/x
  $ cat _build/default/x
  hello
  $ cat _build/default/foo/y
  world
