NAME=netkat_equiv

all: build

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

build: setup.data setup.ml
	ocaml setup.ml -build

install: setup.data setup.ml
	ocaml setup.ml -install

clean:
	ocaml setup.ml -distclean

