  $ dune runtest --display short
  Error: exception Failure("DYNAMIC-ACTION")
  Backtrace:
  Raised at file "stdlib.ml", line 29, characters 17-33
  Called from file "src/dune_lang/dune_lang.ml", line 596, characters 5-8
  Called from file "src/dune_lang/dune_lang.ml", line 884, characters 27-41
  Called from file "src/dune_lang/dune_lang.ml" (inlined), line 714, characters 24-53
  Called from file "src/dune_lang/dune_lang.ml", line 892, characters 4-669
  Called from file "src/dune_lang/dune_lang.ml", line 845, characters 20-32
  Called from file "src/dune_lang/dune_lang.ml", line 985, characters 25-39
  Called from file "src/dune_lang/dune_lang.ml", line 1104, characters 19-30
  Called from file "src/dune_lang/dune_lang.ml", line 1103, characters 19-30
  Called from file "src/dune_lang/dune_lang.ml", line 1103, characters 19-30
  Called from file "src/dune_lang/dune_lang.ml", line 1103, characters 19-30
  Called from file "src/dune_lang/dune_lang.ml", line 595, characters 19-30
  Called from file "src/dune_lang/dune_lang.ml", line 1035, characters 23-62
  Called from file "src/dune_lang/dune_lang.ml", line 595, characters 19-30
  Called from file "src/dune_lang/dune_lang.ml", line 884, characters 27-41
  Called from file "src/dune_lang/dune_lang.ml" (inlined), line 714, characters 24-53
  Called from file "src/dune_lang/dune_lang.ml", line 892, characters 4-669
  Called from file "src/dune_lang/dune_lang.ml", line 690, characters 15-31
  Called from file "list.ml", line 103, characters 22-25
  Called from file "src/stdune/list.ml" (inlined), line 5, characters 19-33
  Called from file "src/stdune/list.ml", line 40, characters 29-39
  Called from file "src/dune/dune_file.ml", line 2237, characters 4-50
  Called from file "src/dune/dune_file.ml", line 2265, characters 8-102
  Called from file "src/dune/dune_load.ml", line 14, characters 18-51
  Called from file "src/dune/dune_load.ml", line 240, characters 8-91
  Called from file "src/dune/dune_load.ml", line 290, characters 26-65
  Called from file "src/dune/dune_load.ml", line 295, characters 19-49
  Called from file "src/dune/main.ml", line 41, characters 13-44
  Called from file "bin/import.ml", line 48, characters 4-25
  Called from file "bin/main.ml", line 7, characters 17-34
  Called from file "src/fiber/fiber.ml", line 114, characters 10-15
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
