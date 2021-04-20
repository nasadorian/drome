BUILD = ocamlbuild -use-ocamlfind

all: io io_tests util typeclasses instances doc

io: io.ml
	$(BUILD) io.byte

io_tests: io_tests.ml io
	$(BUILD) io_tests.byte

util: util.ml
	$(BUILD) util.byte

typeclasses: typeclasses.ml util
	$(BUILD) typeclasses.byte

instances: instances.ml typeclasses
	$(BUILD) instances.byte

doc: writeup.md
	pandoc -f markdown -o writeup.pdf writeup.md

test: io_tests
	./io_tests.byte

clean:
	rm -rf *.byte; rm writeup.pdf
