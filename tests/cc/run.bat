@call python tools/testpp/testpp.py ../../src/generators/gencc.ml gencctest.ml gencctest__.ml
@ocaml ^
-I /lib/ocaml unix.cma ^
::-I /lib/ocaml/site-lib/fileutils fileutils.cma ^
-I /lib/ocaml/site-lib/oUnit oUnit.cma ^
-I libs/extLib libs/extlib/extLib.cma ^
../../_build/src/core/ast.cmo  ../../_build/src/core/type.cmo ^
gencctest__.ml