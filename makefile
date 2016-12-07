# --- Makefile ---
# Authors Brun/Volkel
# Version Beta 1
#
# When adding a library that is not standard,
# you have to modify the _tags file.
# For more info look at:
# https://ocaml.org/learn/tutorials/ocamlbuild/Using_ocamlfind_with_ocamlbuild.html 

OCAMLBUILD = ocamlbuild -use-ocamlfind
OCAMLDEP = ocamldep
OCAMLBUILDFLAGS = -cflag 
PACKAGES = -pkgs xml-light

EXE = main.native
MLIS = IO.mli dynArray.mli graphLib.mli dijkstra2.mli cont.mli drawMap.mli \
	getHttp.mli GUI.mli webCode.mli application.mli simpleUI.mli highlayer.mli 
MLS = main.ml $(MLIS:%.mli=%.ml) 

SRC_DIR = ./src

all: $(EXE:%=$(SRC_DIR)/%)

$(EXE:%=$(SRC_DIR)/%): $(MLS:%=$(SRC_DIR)/%) $(MLIS:%=$(SRC_DIR)/%)
	$(OCAMLBUILD) -Is $^ $(PACKAGES) $@ 

#depend:
#	$(OCAMLDEP) *.mli *.ml > .depend
#
#include .depend

clean:
	rm -rf _build/ $(EXE)
