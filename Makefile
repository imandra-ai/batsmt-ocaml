
build:
	@dune build @install --profile=release

clean:
	@dune clean

doc:
	@dune build @doc

test:
	@dune runtest --force --no-buffer


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

all: build test

reindent:
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -print0 | xargs -0 ocp-indent -i

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		make build-dev ; \
	done

.PHONY: prebuild check release clean

