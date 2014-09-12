// This code is in the public domain - feel free to do anything you
// wish with it.
//
// Eli Bendersky (eliben@gmail.com)
//

#ifndef NFA_H
#define NFA_H

#include "common.h"


class NFA
{
public:
    // Constructed with the NFA size (amount of states), the
    // initial state and the final state
    //
    NFA(unsigned size_, state initial_, state final_);

    NFA(const NFA& other) :
            initial(other.initial), final(other.final),
            size(other.size), trans_table(other.trans_table), 
            inputs(other.inputs)
    {}

    NFA& operator=(const NFA& other);

    // Adds a transition between two states
    //
    void add_trans(state from, state to, input in);

    // Fills states 0 up to other.size with other's states
    //
    void fill_states(const NFA& other);

    // Renames all the NFA's states:
    //
    // For each nfa state: number += shift
    //
    // Functionally, this doesn't affect the NFA,
    // it only makes it larger and renames its states
    //
    void shift_states(unsigned shift);

    // Appends a new, empty state to the NFA
    //
    void append_empty_state();

    // Returns a set of NFA states from which there is a transition on input
    // symbol inp from some state s in states
    //
    set<state> move(set<state> states, input inp);

    // Prints out the NFA
    //
    void show();

    bool is_legal_state(state s);

    state initial;
    state final;
    unsigned size;
    vector<vector<input> > trans_table;

    // All the inputs this nfa responds to
    //
    set<input> inputs;
};


// NFA building functions
//
NFA build_nfa_basic(input in);
NFA build_nfa_alter(NFA nfa1, NFA nfa2);
NFA build_nfa_concat(NFA nfa1, NFA nfa2);
NFA build_nfa_star(NFA nfa);

#endif // NFA_H
