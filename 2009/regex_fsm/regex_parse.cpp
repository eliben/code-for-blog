// This code is in the public domain - feel free to do anything you
// wish with it.
//
// Eli Bendersky (eliben@gmail.com)
//

#include <iostream>
#include <string>
#include <cstdlib>
#include <cctype>
#include "nfa.h"
#include "subset_construct.h"


//
// The BNF for our simple regexes is:
//
// expr     ::= concat '|' expr
//          |   concat
//
// concat   ::= rep . concat
//          |   rep
//
// rep      ::= atom '*'
//          |   atom '?'
//          |   atom
//
// atom     ::= chr
//          |   '(' expr ')'
//
// char     ::= alphanumeric character
//

using namespace std;


// A singleton scanner class, encapsulates the input stream
//
class scanner
{
public:
    void init(string data_)
    {
        data = preprocess(data_);
        next = 0;
    }

    char peek(void)
    {
        return (next < data.size()) ? data[next] : 0;
    }

    char pop(void)
    {
        char cur = peek();

        if (next < data.size())
            ++next;

        return cur;
    }

    unsigned get_pos(void)
    {
        return next;
    }

    friend scanner& my_scanner(void);

private:
    scanner()
    {}

    string preprocess(string in);

    string data;
    unsigned next;
};


// Generates concatenation chars ('.') where
// appropriate
//
string scanner::preprocess(string in)
{
    string out = "";

    string::const_iterator c = in.begin(), up = c + 1;

    // in this loop c is the current char of in, up is the next one
    //
    for (; up != in.end(); ++c, ++up)
    {
        out.push_back(*c);

        if ((isalnum(*c) || *c == ')' || *c == '*' || *c == '?') &&
            (*up != ')' && *up != '|' && *up != '*' && *up != '?'))
            out.push_back('.');
    }

    // don't forget the last char ...
    //
    if (c != in.end())
        out.push_back(*c);

    return out;
}


scanner& my_scanner(void)
{
    static scanner my_scan;
    return my_scan;
}



typedef enum {CHR, STAR, QUESTION, ALTER, CONCAT} node_type;


// Parse node
//
struct parse_node
{
    parse_node(node_type type_, char data_, parse_node* left_, parse_node* right_)
            : type(type_), data(data_), left(left_), right(right_)
    {}

    node_type type;
    char data;
    parse_node* left;
    parse_node* right;
};


parse_node* expr();


void print_tree(parse_node* node, unsigned offset)
{
    if (!node)
        return;

    for (unsigned i = 0; i < offset; ++i)
        cout << " ";

    switch (node->type)
    {
    case CHR:
        cout << node->data;
        break;
    case ALTER:
        cout << '|';
        break;
    case CONCAT:
        cout << '.';
        break;
    case QUESTION:
        cout << '?';
        break;
    case STAR:
        cout << '*';
        break;
    default:
        assert(0);
    }

    cout << endl;

    print_tree(node->left, offset + 4);
    print_tree(node->right, offset + 4);
}


NFA tree_to_nfa(parse_node* tree)
{
    assert(tree);

    switch (tree->type)
    {
    case CHR:
        return build_nfa_basic(tree->data);
    case ALTER:
        return build_nfa_alter(tree_to_nfa(tree->left), tree_to_nfa(tree->right));
    case CONCAT:
        return build_nfa_concat(tree_to_nfa(tree->left), tree_to_nfa(tree->right));
    case STAR:
        return build_nfa_star(tree_to_nfa(tree->left));
    case QUESTION:
        return build_nfa_alter(tree_to_nfa(tree->left), build_nfa_basic(EPS));
    default:
        assert(0);
    }
}


// RD parser
//

// char   ::= alphanumeric character
//
parse_node* chr()
{
    char data = my_scanner().peek();

    if (isalnum(data) || data == 0)
    {
        return new parse_node(CHR, my_scanner().pop(), 0, 0);
    }

    cerr 	<< "Parse error: expected alphanumeric, got "
    <<  my_scanner().peek() << " at #" << my_scanner().get_pos() << endl;
    exit(1);
}


// atom ::= chr
//      |   '(' expr ')'
//
parse_node* atom()
{
    parse_node* atom_node;

    if (my_scanner().peek() == '(')
    {
        my_scanner().pop();
        atom_node = expr();

        if (my_scanner().pop() != ')')
        {
            cerr << "Parse error: expected ')'" << endl;
            exit(1);
        }
    }
    else
    {
        atom_node = chr();
    }

    return atom_node;
}


// rep  ::= atom '*'
//      |   atom '?'
//      |   atom
//
parse_node* rep()
{
    parse_node* atom_node = atom();

    if (my_scanner().peek() == '*')
    {
        my_scanner().pop();

        parse_node* rep_node = new parse_node(STAR, 0, atom_node, 0);
        return rep_node;
    }
    else if (my_scanner().peek() == '?')
    {
        my_scanner().pop();

        parse_node* rep_node = new parse_node(QUESTION, 0, atom_node, 0);
        return rep_node;
    }
    else
    {
        return atom_node;
    }
}


// concat   ::= rep . concat
//          |   rep
//
parse_node* concat()
{
    parse_node* left = rep();

    if (my_scanner().peek() == '.')
    {
        my_scanner().pop();
        parse_node* right = concat();

        parse_node* concat_node = new parse_node(CONCAT, 0, left, right);
        return concat_node;
    }
    else
    {
        return left;
    }
}


// expr ::= concat '|' expr
//      |   concat
//
parse_node* expr(void)
{
    parse_node* left = concat();

    if (my_scanner().peek() == '|')
    {
        my_scanner().pop();
        parse_node* right = expr();

        parse_node* expr_node = new parse_node(ALTER, 0, left, right);
        return expr_node;
    }
    else
    {
        return left;
    }
}


int main(int argc, char** argv)
{
    if (argc != 3)
    {
        cerr << "Usage: " << argv[0] << " <regex> <string>" << endl;
        exit(1);
    }

    my_scanner().init(argv[1]);

    parse_node* n = expr();

    if (my_scanner().peek() != 0)
    {
        cerr    << "Parse error: unexpected char " << my_scanner().peek() 
                << " at #" << my_scanner().get_pos() << endl;
        exit(1);
    }

    NFA nfa = tree_to_nfa(n);

    nfa.show();

    DFA dfa = subset_construct(nfa);
    cout << endl << endl;
    dfa.show();

    cout << endl << endl;

    cout << "Result: " << dfa.simulate(argv[2]) << endl;

    return 0;
}
