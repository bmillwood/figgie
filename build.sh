#!/bin/bash -eu
ocamlbuild -use-ocamlfind -plugin-tag "package(ppx_driver.ocamlbuild)" \
	figgie/figgie.byte web/web.byte
js_of_ocaml +bin_prot.js +core_kernel.js +nat.js +weak.js web.byte
