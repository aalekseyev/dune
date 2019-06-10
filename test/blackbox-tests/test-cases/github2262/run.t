  $ echo '(lang dune 1.10)' > dune-project

  $ echo 'let x = 8' > future_example.ml

  $ cat > dune <<EOF
  > (library
  >  (name future_example)
  >  (preprocess (pps -- -pp %{bin:ocaml-syntax-shims}))
  >  )
  > 
  > EOF

  $ dune build @all
  File "dune", line 3, characters 27-50:
  3 |  (preprocess (pps -- -pp %{bin:ocaml-syntax-shims}))
                                 ^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{bin:..} isn't allowed in this position
  [1]

