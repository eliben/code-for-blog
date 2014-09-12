A C++ implementation of a regular expression recognizer. The recognizer constructs an NFA from a regular expression (with a simplified syntax), converts it into a DFA, and simulates the DFA on the input string.

This code accompanies a set of 6 articles I've written for GameDev (www.gamedev.net), which have also been collected into a chapter of the book "Advanced game programming - a gamedev.net collection".

The main function is in regex_parse.cpp

To compile/build, run:
g++ -ansi -Wall -pedantic -o regex *.cpp

The code is ANSI C++ and should compile cleanly on any standard C++ compiler.


This code is in the public domain
Eli Bendersky (eliben@gmail.com)
