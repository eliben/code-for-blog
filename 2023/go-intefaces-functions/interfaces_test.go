// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package interfaces

import (
	"slices"
	"strconv"
	"testing"
)

func TestBinaryTreeSuccessors(t *testing.T) {
	// finite binary tree values will be limited to 10
	var tests = []struct {
		s          State
		wantFull   States
		wantFinite States
	}{
		{1, States{2, 3}, States{2, 3}},
		{3, States{6, 7}, States{6, 7}},
		{4, States{8, 9}, States{8, 9}},
		{5, States{10, 11}, States{10}},
		{10, States{20, 21}, States{}},
	}

	for _, tt := range tests {
		t.Run(strconv.Itoa(int(tt.s)), func(t *testing.T) {
			succ := binaryTree(tt.s)
			if !slices.Equal(succ, tt.wantFull) {
				t.Errorf("full binary tree: got %v, want %v", succ, tt.wantFull)
			}

			succFinite := finiteBinaryTree(10)(tt.s)
			if !slices.Equal(succFinite, tt.wantFinite) {
				t.Errorf("finite binary tree: got %v, want %v", succFinite, tt.wantFinite)
			}
		})
	}
}

func TestInterfacesSearchFinite50(t *testing.T) {
	treeLimit := 50
	tree := finiteBinaryTree(State(treeLimit))

	for i := 1; i < treeLimit*2; i++ {
		wantFound := i <= treeLimit

		bfsFound := bfsTreeSearch(1, stateIs(State(i)), tree) != -1
		dfsFound := dfsTreeSearch(1, stateIs(State(i)), tree) != -1
		bestFound := bestCostTreeSearch(1, stateIs(State(i)), tree, costDiffTarget(State(i))) != -1

		t.Run(strconv.Itoa(i), func(t *testing.T) {
			if wantFound != dfsFound {
				t.Errorf("got bfs found %v, want %v", bfsFound, wantFound)
			}
			if wantFound != dfsFound {
				t.Errorf("got dfs found %v, want %v", dfsFound, wantFound)
			}
			if wantFound != bestFound {
				t.Errorf("got best found %v, want %v", bestFound, wantFound)
			}
		})
	}
}
