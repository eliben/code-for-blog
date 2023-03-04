// Shows the use of higher-order functions in Go; reproduces the "searching
// tools" samples from chapter 6 of PAIP.
//
// Eli Bendersky [https://eli.thegreenplace.net]
// This code is in the public domain.
package functypes

import "sort"

type State int
type States []State

// GoalP takes a state and determines whether it's a goal state.
type GoalP func(s State) bool

// Successors returns the successors of a state.
type Successors func(s State) States

// Combiner determines the search strategy by combining successors of the
// current state with all the other states into a single list of states.
type Combiner func(succ States, others States) States

// treeSearch returns the state if it's found in the tree; returns -1 if such a
// state wasn't found.
func treeSearch(states States, goalp GoalP, succ Successors, combiner Combiner) State {
	//log.Println("states:", states)
	if len(states) == 0 {
		return -1
	}

	first := states[0]
	if goalp(first) {
		return first
	} else {
		return treeSearch(combiner(succ(first), states[1:]), goalp, succ, combiner)
	}
}

// appendOthers is a Combiner function that appends others to succ.
func appendOthers(succ States, others States) States {
	return append(succ, others...)
}

// prependOthers is a Combiner function that prepends others to succ.
func prependOthers(succ States, others States) States {
	return append(others, succ...)
}

// stateIs returns a GoalP that checks a state for equality with n.
func stateIs(n State) GoalP {
	return func(s State) bool { return n == s }
}

func binaryTree(s State) States {
	return []State{s * 2, s*2 + 1}
}

func finiteBinaryTree(n State) Successors {
	return func(s State) States {
		return filter(binaryTree(s), func(item State) bool { return item <= n })
	}
}

func bfsTreeSearch(start State, goalp GoalP, succ Successors) State {
	return treeSearch(States{start}, goalp, succ, prependOthers)
}

func dfsTreeSearch(start State, goalp GoalP, succ Successors) State {
	return treeSearch(States{start}, goalp, succ, appendOthers)
}

type CostFunc func(s State) int

// costDiffTarget creates a cost function that uses numerical distance from `n`
// as the cost.
func costDiffTarget(n State) CostFunc {
	return func(s State) int {
		delta := int(s) - int(n)
		if delta < 0 {
			return -delta
		} else {
			return delta
		}
	}
}

func sorter(cost CostFunc) Combiner {
	return func(succ States, others States) States {
		all := append(succ, others...)
		sort.Slice(all, func(i, j int) bool {
			return cost(all[i]) < cost(all[j])
		})
		return all
	}
}

func bestCostTreeSearch(start State, goalp GoalP, succ Successors, cost CostFunc) State {
	return treeSearch(States{start}, goalp, succ, sorter(cost))
}

// filter filters a slice based on a predicate, returning a new slice whose
// elements fulfill the predicate.
func filter[T any](s []T, pred func(item T) bool) []T {
	var result []T
	for _, item := range s {
		if pred(item) {
			result = append(result, item)
		}
	}
	return result
}
