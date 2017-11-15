python tools/testpp/testpp.py ../../src/generators/gencc.ml gencctest.ml gencctest.pp.ml
ocaml \
-I /lib/ocaml unix.cma \
-I libs/ounit/_build/src oUnit.cma \
-I ~/.opam/system/lib/extlib extLib.cma \
-I ../../libs/extlib-leftovers extlib-leftovers.cma \
-I ../../_build/src/core \
globals.cmo \
meta.cmo \
ast.cmo \
type.cmo \
gencctest.pp.ml