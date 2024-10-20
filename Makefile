lsp:
	cd rock-lsp && RUST_LOG=lsp_server=debug cargo run

default:
	make -C tree-sitter-rock all
	cd rock-fmt && cargo run -- ../tree-sitter-rock/examples/function.rock
