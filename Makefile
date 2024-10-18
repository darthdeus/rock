default:
	make -C tree-sitter-rock all
	cd rock-fmt && cargo run -- ../tree-sitter-rock/examples/function.rock
