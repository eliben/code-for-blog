package interfaces

import (
	"strconv"
	"testing"
)

func TestInterfacesSearchFinite30(t *testing.T) {
	treeLimit := 30
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
