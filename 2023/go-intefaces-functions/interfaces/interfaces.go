package interfaces

import "sort"

type State int
type States []State

// GoalDetector is an interface that wraps a single IsGoal method. IsGoal
// takes a state and determines whether it's a goal state.
type GoalDetector interface {
	IsGoal(s State) bool
}

// SuccessorGenerator is an interface that wraps a single Successors method.
// Successors returns the successors of a state.
type SuccessorGenerator interface {
	Successors(s State) States
}

// Combiner is an interface that wrapes a single Combine method. Combine
// determines the search strategy by combining successors of the current state
// with all the other states into a single list of states.
type Combiner interface {
	Combine(succ States, others States) States
}

// treeSearch returns the state if it's found in the tree; returns -1 if such a
// state wasn't found.
func treeSearch(states States, gd GoalDetector, sg SuccessorGenerator, combiner Combiner) State {
	//fmt.Println("states:", states)
	if len(states) == 0 {
		return -1
	}

	first := states[0]
	if gd.IsGoal(first) {
		return first
	} else {
		return treeSearch(combiner.Combine(sg.Successors(first), states[1:]), gd, sg, combiner)
	}
}

// Function --> interface adapters, similar to net/http.HandlerFunc

type IsGoalFunc func(State) bool

func (f IsGoalFunc) IsGoal(s State) bool {
	return f(s)
}

type SuccessorsFunc func(State) States

func (f SuccessorsFunc) Successors(s State) States {
	return f(s)
}

type CombineFunc func(States, States) States

func (f CombineFunc) Combine(succ States, others States) States {
	return f(succ, others)
}

func appendOthers(succ States, others States) States {
	return append(succ, others...)
}

func prependOthers(succ States, others States) States {
	return append(others, succ...)
}

func stateIs(n State) IsGoalFunc {
	return func(s State) bool { return n == s }
}

func binaryTree(s State) States {
	return []State{s * 2, s*2 + 1}
}

func finiteBinaryTree(n State) SuccessorsFunc {
	return func(s State) States {
		return filter(binaryTree(s), func(item State) bool { return item <= n })
	}
}

func bfsTreeSearch(start State, gd GoalDetector, sg SuccessorGenerator) State {
	return treeSearch(States{start}, gd, sg, CombineFunc(prependOthers))
}

func dfsTreeSearch(start State, gd GoalDetector, sg SuccessorGenerator) State {
	return treeSearch(States{start}, gd, sg, CombineFunc(appendOthers))
}

type CostFunc func(s State) int

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
	return CombineFunc(func(succ States, others States) States {
		all := append(succ, others...)
		sort.Slice(all, func(i, j int) bool {
			return cost(all[i]) < cost(all[j])
		})
		return all
	})
}

func bestCostTreeSearch(start State, gd GoalDetector, sg SuccessorGenerator, cost CostFunc) State {
	return treeSearch(States{start}, gd, sg, sorter(cost))
}

// filter filters a slice based on a predicate.
func filter[T any](s []T, pred func(item T) bool) []T {
	var result []T
	for _, item := range s {
		if pred(item) {
			result = append(result, item)
		}
	}
	return result
}
