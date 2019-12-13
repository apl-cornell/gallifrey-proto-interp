default:
	ocamlbuild -tag 'debug' -use-ocamlfind -tag thread main.native ; mv main.native gallifrey

bisect:
	BISECT_COVERAGE=YES ocamlbuild -tag 'debug' -use-ocamlfind -tag thread main.native ; mv main.native gallifrey; ./gallifrey -test; make report

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli gallifrey _build/* *.out *.html _coverage/*

test:
	ocamlbuild -tag 'debug' -use-ocamlfind -pkgs core -tag thread main.native ; mv main.native gallifrey; ./gallifrey -test

report:
	bisect-ppx-report -html _coverage/ -I _build/ bisect*.out; open _coverage/index.html