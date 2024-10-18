package tree_sitter_rock_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_rock "github.com/darthdeus/rock/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_rock.Language())
	if language == nil {
		t.Errorf("Error loading Rock grammar")
	}
}
