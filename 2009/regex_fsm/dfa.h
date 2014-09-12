// This code is in the public domain - feel free to do anything you
// wish with it.
//
// Eli Bendersky (eliben@gmail.com)
//

#ifndef DFA_H
#define DFA_H

#include "common.h"


class DFA
{
public:
    typedef pair<state, input> transition;

    // transition table, set of final states and the start state
    //
    map<transition, state> trans_table;
    state start;
    set<state> final;

    string simulate(string in)
    {
        state cur_state = start;

        for (string::const_iterator i = in.begin(); i != in.end(); ++i)
        {
            transition trans = make_pair(cur_state, *i);

            if (trans_table.find(trans) == trans_table.end())
                return "REJECT";

            cur_state = trans_table[trans];
        }

        if (final.find(cur_state) != final.end())
            return "ACCEPT";
        else
            return "REJECT";
    }

    void show()
    {
        cout << "DFA start state: " << start << endl;
        cout << "DFA final state(s): ";

        for (set<state>::const_iterator i = final.begin(); i != final.end(); ++i)
            cout << *i << " ";
        cout << endl;

        for (   map<transition, state>::const_iterator i = trans_table.begin(); 
                i != trans_table.end(); ++i)
        {
            cout << "Trans[" << (i->first).first << ", " << (i->first).second << "] = " << i->second << endl;
        }
    }
};



#endif // DFA_H
