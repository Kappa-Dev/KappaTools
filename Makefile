## see: http://www.ocaml.info/home/ocaml_sources.html#toc16

## put here the names of your source files (in the right order)
SOURCES = \
		dataStructures/largeArray.ml dataStructures/memoryManagement.ml dataStructures/longString.ml dataStructures/heap.ml  \
		dataStructures/tools.ml main/parameter.ml dataStructures/mapExt.ml dataStructures/valMap.ml dataStructures/mods.ml dataStructures/random_tree.ml \
		dataStructures/liftSet.ml \
		Error/debug.ml Error/ExceptionDefn.ml \
		grammar/ast.ml grammar/kappaParser.mly grammar/kappaLexer.mll \
		pattern/signature.ml pattern/environment.ml \
		siteGraphs/node.ml pattern/mixture.mli  \
		pattern/mixture.ml pattern/precondition.ml pattern/precondition.mli \
		siteGraphs/graph.ml siteGraphs/species.ml pattern/matching.ml pattern/dynamics.ml \
		grammar/eval.mli simulation/nonLocal.ml simulation/state.ml simulation/external.ml grammar/eval.ml  \
		cflow/causal.ml simulation/plot.ml simulation/run.ml\
		main/main.ml 

## the name of the resulting executable
RESULT  = KaSim

## generate type information (.annot files)
ANNOTATE = no

## make target (see manual) : byte-code, debug-code, native-code, ...
all: native-code

##if ocamlopt.opt is not in your path, 
##uncomment the line below and set value below according to the location of your ocaml compilers 
##(usually /usr/bin/ under linux and /sw/bin under MAC OS X)

#OCAMLBINPATH = /usr/bin/

OCAMLCP = $(OCAMLBINPATH)ocamlcp
OCAMLLEX = $(OCAMLBINPATH)ocamllex
OCAMLYACC = $(OCAMLBINPATH)ocamlyacc
OCAMLC = $(OCAMLBINPATH)ocamlc.opt 
OCAMLOPT = $(OCAMLBINPATH)ocamlopt.opt #-g -ccopt -g
OCAMLDEP = $(OCAMLBINPATH)ocamldep
CC = gcc

include OCamlMakefile
