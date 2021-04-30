BUILD = ocamlbuild -use-ocamlfind

all: drome io_base io_tests util typeclasses instances doc

drome: drome.ml io_base util
	$(BUILD) drome.byte

io_base: io_base.ml
	$(BUILD) io_base.byte

io_tests: io_tests.ml io_base drome
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
