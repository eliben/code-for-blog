// This code is in the public domain - feel free to do anything you
// wish with it.
//
// Eli Bendersky (eliben@gmail.com)
//

#include <iostream>
#include <cassert>
#include "nfa.h"


using namespace std;


/////////////////////////////////////////////////////////////////
//
// Implementation of the NFA class
//


NFA::NFA(unsigned size_, state initial_, state final_)
        : initial(initial_), final(final_), size(size_)
{
    assert(is_legal_state(initial));
    assert(is_legal_state(final));

    vector<input> empty_row(size, NONE);

    // Initialize trans_table with an "empty graph", no transitions
    // between its states
    //
    for (unsigned i = 0; i < size; ++i)
        trans_table.push_back(empty_row);
}


NFA& NFA::operator=(const NFA& other)
{
    if (this == &other)
        return *this;

    initial = other.initial;
    final = other.final;
    size = other.size;
    trans_table = other.trans_table;

    return *this;
}


bool NFA::is_legal_state(state s)
{
    // We have 'size' states, numbered 0 to size-1
    //
    if (s < 0 || s >= size)
        return false;

    return true;
}


void NFA::add_trans(state from, state to, input in)
{
    assert(is_legal_state(from));
    assert(is_legal_state(to));

    trans_table[from][to] = in;

    if (in != EPS)
        inputs.insert(in);
}


void NFA::shift_states(unsigned shift)
{
    unsigned new_size = size + shift;

    if (shift < 1)
        return;

    // create a new, empty transition table (of the new size)
    //
    vector<input> empty_row(new_size, NONE);
    vector<vector<input> > new_trans_table(new_size, empty_row);

    // copy all the transitions to the new table, at their
    // new locations
    //
    for (state i = 0; i < size; ++i)
    {
        for (state j = 0; j < size; ++j)
        {
            new_trans_table[i + shift][j + shift] = trans_table[i][j];
        }
    }

    // update the NFA members
    //
    size = new_size;
    initial += shift;
    final += shift;
    trans_table = new_trans_table;
}


void NFA::fill_states(const NFA& other)
{
    for (state i = 0; i < other.size; ++i)
    {
        for (state j = 0; j < other.size; ++j)
        {
            trans_table[i][j] = other.trans_table[i][j];
        }
    }

    for (   set<input>::const_iterator i = other.inputs.begin(); 
            i != other.inputs.end(); ++i)
        inputs.insert(*i);
}


void NFA::append_empty_state()
{
    // append a new row (already with a larger size)
    //
    vector<input> empty_row(size + 1, NONE);
    trans_table.push_back(empty_row);

    // append a new column
    //
    for (state i = 0; i < size; ++i)
        trans_table[i].push_back(NONE);

    size += 1;
}


void NFA::show()
{
    cout << "This NFA has " << size << " states: 0 - " << size - 1 << endl;
    cout << "The initial state is " << initial << endl;
    cout << "The final state is " << final << endl << endl;

    for (state from = 0; from < size; ++from)
    {
        for (state to = 0; to < size; ++to)
        {
            input in = trans_table[from][to];

            if (in != NONE)
            {
                cout << "Transition from " << from << " to " << to << " on input ";

                if (in == EPS)
                {
                    cout << "EPS" << endl;
                }
                else
                {
                    cout << in << endl;
                }
            }
        }
    }

    cout << endl;
}


set<state> NFA::move(set<state> states, input inp)
{
    set<state> result;

    // for each state in the set of states
    //
    for (   set<state>::const_iterator state_i = states.begin(); 
            state_i != states.end(); ++state_i)
    {
        // for each transition from this state
        //
        for (   vector<input>::const_iterator trans_i = trans_table[*state_i].begin(); 
                trans_i != trans_table[*state_i].end(); ++trans_i)
        {
            // if the transition is on input inp, add it to the resulting set
            //
            if (*trans_i == inp)
            {
                state u = trans_i - trans_table[*state_i].begin();
                result.insert(u);
            }
        }
    }

    return result;
}


/////////////////////////////////////////////////////////////////
//
// NFA building functions
//
// Using Thompson Construction, build NFAs from basic inputs or
// compositions of other NFAs.
//


// Builds a basic, single input NFA
//
NFA build_nfa_basic(input in)
{
    NFA basic(2, 0, 1);
    basic.add_trans(0, 1, in);

    return basic;
}


// Builds an alternation of nfa1 and nfa2 (nfa1|nfa2)
//
NFA build_nfa_alter(NFA nfa1, NFA nfa2)
{
    // How this is done: the new nfa must contain all the states in
    // nfa1 and nfa2, plus a new initial and final states.
    // First will come the new initial state, then nfa1's states, then
    // nfa2's states, then the new final state
    //

    // make room for the new initial state
    nfa1.shift_states(1);

    // make room for nfa1
    nfa2.shift_states(nfa1.size);

    // create a new nfa and initialize it with (the shifted)
    // nfa2
    //
    NFA new_nfa(nfa2);

    // nfa1's states take their places in new_nfa
    //
    new_nfa.fill_states(nfa1);

    // Set new initial state and the transitions from it
    //
    new_nfa.add_trans(0, nfa1.initial, EPS);
    new_nfa.add_trans(0, nfa2.initial, EPS);
    new_nfa.initial = 0;

    // Make up space for the new final state
    //
    new_nfa.append_empty_state();

    // Set new final state
    //
    new_nfa.final = new_nfa.size - 1;
    new_nfa.add_trans(nfa1.final, new_nfa.final, EPS);
    new_nfa.add_trans(nfa2.final, new_nfa.final, EPS);

    return new_nfa;
}


// Builds a concatenation of nfa1 and nfa2 (nfa1nfa2)
//
NFA build_nfa_concat(NFA nfa1, NFA nfa2)
{
    // How this is done: First will come nfa1, then nfa2 (its
    // initial state replaced with nfa1's final state)
    //
    nfa2.shift_states(nfa1.size - 1);

    // create a new nfa and initialize it with (the shifted)
    // nfa2
    //
    NFA new_nfa(nfa2);

    // nfa1's states take their places in new_nfa
    // note: nfa1's final state overwrites nfa2's initial state,
    // thus we get the desired merge automagically (the transition
    // from nfa2's initial state now transits from nfa1's final state)
    //
    new_nfa.fill_states(nfa1);

    // set the new initial state (the final state stays nfa2's final state,
    // and was already copied)
    //
    new_nfa.initial = nfa1.initial;

    return new_nfa;
}


// Builds a star (kleene closure) of nfa (nfa*)
//
NFA build_nfa_star(NFA nfa)
{
    // How this is done: First will come the new initial state,
    // then nfa, then the new final state
    //

    // make room for the new initial state
    //
    nfa.shift_states(1);

    // make room for the new final state
    //
    nfa.append_empty_state();

    // add new transitions
    //
    nfa.add_trans(nfa.final, nfa.initial, EPS);
    nfa.add_trans(0, nfa.initial, EPS);
    nfa.add_trans(nfa.final, nfa.size - 1, EPS);
    nfa.add_trans(0, nfa.size - 1, EPS);

    nfa.initial = 0;
    nfa.final = nfa.size - 1;

    return nfa;
}






