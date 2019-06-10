  $ echo '(lang dune 1.10)' > dune-project

  $ echo 'let x = 8' > future_example.ml

  $ mkdir ppx_whatever
  $ cat > ppx_whatever/dune <<EOF
  > (library
  > (name ppx_whatever)
  > (ppx.driver (main "(fun () ->
  >    Printf.printf \"let x = 10\")"))
  > )
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name future_example)
  >  (preprocess (pps ppxlib -- -pp ./ocaml-syntax-shims))
  >  (preprocessor_deps ./ocaml-syntax-shims)
  >  )
  > (rule (copy %{bin:ocaml-syntax-shims} ./ocaml-syntax-shims))
  > EOF

  $ dune build @all

