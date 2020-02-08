
  $ cat >dune-project <<EOF
  > (lang dune 2.2)
  > EOF

  $ mkdir w
  $ cat >w/dune <<EOF
  > (has_rules_for_subdirs true)
  > (rule
  >    (targets x foo/y)
  >    (action (progn
  >      (with-stdout-to x (echo hello))
  >      (with-stdout-to foo/y (echo world)))))
  > EOF

  $ dune build _build/default/w/foo/y
  $ cat _build/default/w/x
  hello
  $ cat _build/default/w/foo/y
  world

  $ rm -r _build

  $ dune build _build/default/w/x
  $ cat _build/default/w/x
  hello
  $ cat _build/default/w/foo/y
  world

  $ cat > dune <<EOF
  > (copy_files w/something)
  > (rule (targets w/forbidden allowed) (action (progn (with-stdout-to allowed (echo hello)) (with-stdout-to w/forbidden (echo hello)))))
  > EOF
  $ dune build w/forbidden
  Error: Don't know how to build w/forbidden
  [1]
# CR aalekseyev: [dune build allowed] builds a rule that has a forbidden targets.
# This is bad.
  $ dune build allowed
  $ dune build w/forbidden
  Error: Don't know how to build w/forbidden
  [1]
