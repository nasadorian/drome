BUILD = ocamlbuild -use-ocamlfind

all: drome dsl io_tests resource_tests util typeclasses instances doc

drome: drome.ml dsl util
	$(BUILD) drome.byte

dsl: dsl.ml
	$(BUILD) dsl.byte

io_tests: io_tests.ml dsl drome
	$(BUILD) io_tests.byte

resource_tests: resource_tests.ml drome
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
