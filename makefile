BUILD = ocamlbuild -use-ocamlfind

all: drome dsl io resource refio io_tests resource_tests refio_tests util typeclasses instances doc

io: io.ml util dsl instances
	$(BUILD) io.byte

resource: resource.ml dsl io instances
	$(BUILD) resource.byte

refio: refio.ml dsl io
	$(BUILD) refio.byte

drome: drome.ml dsl util
	$(BUILD) drome.byte

dsl: dsl.ml
	$(BUILD) dsl.byte

io_tests: io_tests.ml dsl drome
	$(BUILD) io_tests.byte

resource_tests: resource_tests.ml drome
	$(BUILD) resource_tests.byte

refio_tests: refio_tests.ml drome
	$(BUILD) refio_tests.byte

util: util.ml
	$(BUILD) util.byte

typeclasses: typeclasses.ml util
	$(BUILD) typeclasses.byte

instances: instances.ml typeclasses
	$(BUILD) instances.byte

doc: writeup.md
	pandoc -f markdown -o writeup.pdf writeup.md

test: resource_tests io_tests refio_tests
	./io_tests.byte && ./resource_tests.byte && ./refio_tests.byte

clean:
	rm -rf *.byte; rm writeup.pdf
