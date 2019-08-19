  $ dune runtest --display short
  Error: exception Failure("ACTION EXEC RUN DYNAMIC")
  Backtrace:
  Raised at file "stdlib.ml", line 29, characters 17-33
  Called from file "src/dune/action_exec.ml", line 227, characters 6-62
  Called from file "src/dune/build_system.ml", line 1446, characters 22-70
  Called from file "src/dune/build_system.ml", line 1445, characters 10-383
  Called from file "src/fiber/fiber.ml", line 114, characters 10-15
  Re-raised at file "src/stdune/exn.ml", line 45, characters 38-65
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 45, characters 38-65
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 45, characters 38-65
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  Re-raised at file "src/stdune/exn.ml", line 45, characters 38-65
  Called from file "src/fiber/fiber.ml", line 85, characters 10-17
  
  I must not segfault.  Uncertainty is the mind-killer.  Exceptions are
  the little-death that brings total obliteration.  I will fully express
  my cases.  Execution will pass over me and through me.  And when it
  has gone past, I will unwind the stack along its path.  Where the
  cases are handled there will be nothing.  Only I will remain.
  [1]
