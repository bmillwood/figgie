#!/bin/bash -eu
ocamlbuild -use-ocamlfind -plugin-tag "package(ppx_driver.ocamlbuild)" \
	figgie/figgie.byte web/observer.byte
js_of_ocaml +bin_prot.js +core_kernel.js +nat.js +weak.js observer.byte
