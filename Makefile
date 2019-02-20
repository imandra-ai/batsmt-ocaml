
build:
	@dune build @install --profile=release

clean:
	@cargo clean
	@dune clean

check:
	@cargo check

doc:
	@dune build @doc

dev: build test

test:
	@dune runtest --force --no-buffer

install: build
	@dune install

# TODO: post vendoring
# must create this manually, since dune won't take it as a dependency
# see https://github.com/ocaml/dune/issues/1407
cargo-config-file:
	@mkdir -p .cargo
	#@cat cargo-config > .cargo/config

CAML_LIB ?= $(shell ocamlc -where)
CARGO_FLAGS = --release 
# TODO: post vendoring
# CARGO_FLAGS = --release --frozen

build-rust-stubs: cargo-config-file
	@if [ "$$(uname)" = "Darwin" ]; then \
		RUSTFLAGS='-L $(CAML_LIB) -lcamlrun' cargo build $(CARGO_FLAGS) ; \
	else \
		cargo build $(CARGO_FLAGS) ; \
  	fi

reindent:
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

watch:
	@dune build @all -w

.PHONY: prebuild check clean

