build:
	ocamlbuild -tag 'debug' -use-ocamlfind -pkgs core -tag thread main.native ; mv main.native gallifrey

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli gallifrey _build/*

test:
	ocamlbuild -tag 'debug' -use-ocamlfind -pkgs core -tag thread main.native ; mv main.native gallifrey; ./gallifrey -test
