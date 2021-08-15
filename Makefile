# MODULES=lib/db lib/dbquery main lib/search testing/unit_test
# OBJECTS=$(MODULES:=.cmo)
# MLS=$(MODULES:=.re)
# MLIS=$(MODULES:=.rei)

default: build
	utop

lock: ## Generate a lock file
	opam lock -y .
	cd lib && opam lock -y .

build:
	@dune build @all

script:
	@dune exec ./bin/script.exe

test:
	@dune clean
	@dune build @dotenvtests/runtest
	@dune build @testing/runtest
	# @dune runtest -j 1

format:
	opam exec -- dune build @fmt --auto-promote

drop:
	rm upick.db

clean:
	@dune clean
	rm -rf doc.public project.zip

docs: clean build
	@dune build @doc
	mkdir -p doc.public
	cp -r _build/default/_doc/_html doc.public

app:
	@dune exec ./bin/main.exe

zip:
	zip project.zip *.re* .ocamlinit .merlin *.rei* dune dune-project *.txt* *.md* *.json _tags Makefile
