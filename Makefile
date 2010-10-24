# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = main/parameter.ml \
		dataStructures/largeArray.ml dataStructures/memoryManagement.ml dataStructures/heap.ml  \
		dataStructures/misc.ml dataStructures/mapExt.ml dataStructures/valMap.ml dataStructures/random_tree.ml dataStructures/mods.ml dataStructures/liftSet.ml\
		Error/debug.ml Error/ExceptionDefn.ml \
		grammar/ast.ml grammar/kappaParser.mly grammar/kappaLexer.mll \
		pattern/signature.ml pattern/environment.ml \
		siteGraphs/node.ml pattern/mixture.mli  \
		pattern/mixture.ml pattern/precondition.ml pattern/precondition.mli \
		siteGraphs/graph.ml siteGraphs/species.ml pattern/matching.ml pattern/dynamics.ml \
		grammar/eval.mli simulation/state.ml simulation/external.ml grammar/eval.ml  \
		simulation/plot.ml simulation/b3.ml simulation/run.ml\
		main/fileName.ml main/main.ml 

# the name of the resulting executable
RESULT  = kaSim

# generate type information (.annot files)
ANNOTATE = yes


# make target (see manual) : byte-code, debug-code, native-code, ...
all: nc


OCAMLBINPATH = /sw/bin
OCAMLLIBPATH = /sw/lib/ocaml
OCAMLCP = $(OCAMLBINPATH)/ocamlcp
OCAMLLEX = $(OCAMLBINPATH)/ocamllex
OCAMLYACC = $(OCAMLBINPATH)/ocamlyacc
OCAMLC = $(OCAMLBINPATH)/ocamlc.opt
OCAMLOPT = $(OCAMLBINPATH)/ocamlopt.opt #-g -ccopt -g
OCAMLDEP = $(OCAMLBINPATH)/ocamldep
CC = gcc

include OCamlMakefile