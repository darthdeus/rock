.PHONY: stick

lsp-watchexec:
	cd rock-lsp && watchexec cargo build

lsp:
	cd rock-lsp && RUST_LOG=lsp_server=debug cargo run

default:
	make -C tree-sitter-rock all
	cd rock-fmt && cargo run -- ../tree-sitter-rock/examples/function.rock

stick:
	watchexec -r -i tree-sitter-rock/examples/function.rock cargo run --bin stick server
