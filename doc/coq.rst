.. _coq-main:

******
Coq
******

Dune is also able to build Coq developments. A Coq project is a mix of
Coq ``.v`` files and (optionally) OCaml libraries linking to the Coq
API (in which case we say the project is a *Coq plugin*). To enable
Coq support in a dune project, the language version should be selected
in the ``dune-project`` file. For example:

.. code:: scheme

    (using coq 0.1)

This will enable support for the ``coqlib`` stanza in the current project. If the
language version is absent, dune will automatically add this line with the
latest Coq version to the project file once a ``(coqlib ...)`` stanza is used anywhere.


Basic Usage
===========

The basic form for defining Coq libraries is very similar to the OCaml form:

.. code:: scheme

    (coqlib
     (name <module_prefix>)
     (synopsis <text>)
     (modules <ordered_set_lang>)
     (flags <coq_flags>))

The stanza will build all `.v` files on the given directory.
The semantics of fields is:
- ``<module_prefix>>`` will be used as the default Coq library prefix
  ``-R``
- the ``modules`` field does allow to constraint the set of modules
  included in the library, similarly to its OCaml counterpart
- ``<coq_flags>`` will be passed to ``coqc``.

Library Composition and Handling
===================

The ``coqlib`` stanza does not yet support composition of Coq
libraries. In the 0.1 version of the language, libraries are located
using Coq's built-in library management, thus Coq will always resort
to the installed version of a particular library.

This will be fixed in the future.

Recursive modules
===================

Adding:

.. code:: scheme
    (include_subdirs unqualified)

to the ``dune`` file will make Dune to consider all the modules in the
current directory and sub-directories, qualified in the current Coq
style.
