BUILD = ocamlbuild -use-ocamlfind

all: io util doc

io: io.ml
	$(BUILD) io.byte

io_tests: io_tests.ml io
	$(BUILD) io_tests.ml

util: util.ml
	$(BUILD) util.byte

doc: writeup.md
	pandoc -f markdown -o writeup.pdf writeup.md
