
  $ cat >dune-project <<EOF
  > (lang dune 2.2)
  > EOF
  $ cat >dune <<EOF
  > (has_rules_for_subdirs true)
  > (rule (targets foo/x y) (action (progn (with-stdout-to foo/x (echo hi)) (with-stdout-to y (echo bye)))))
  > EOF

  $ dune build _build/default/foo/x
