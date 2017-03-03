#!/bin/bash -eux
ocamlbuild -use-ocamlfind -no-links -plugin-tag "package(ppx_driver.ocamlbuild)" \
	{bot,server,web}/main.byte
for thing in bot server web
do
  [ -e "$thing.byte" ] \
    || ln -s "_build/$thing/main.byte" "$thing.byte"
done
js_of_ocaml base.js +bin_prot.js core_kernel.js +nat.js +weak.js web.byte
