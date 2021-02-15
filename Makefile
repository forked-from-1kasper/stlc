default: native

clean:
	ocamlbuild -clean

native:
	ocamlbuild -use-menhir stlc.native

byte:
	ocamlbuild -use-menhir stlc.byte -tag 'debug'
