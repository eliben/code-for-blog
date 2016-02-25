// Example of using std::unordered_map::insert for its pair return value.
//
// Eli Bendersky [http://eli.thegreenplace.net]
// This code is in the public domain.
#include <iostream>
#include <unordered_map>

using map_int_to_string = std::unordered_map<int, std::string>;

void try_insert(map_int_to_string& m, int i, const std::string& s) {
  std::pair<map_int_to_string::iterator, bool> p = m.insert({i, s});

  if (p.second) {
    std::cout << "insertion succeeded. ";
  } else {
    std::cout << "insertion failed. ";
  }

  std::cout << "key=" << p.first->first << " value=" << p.first->second << "\n";
}

void try_insert_with_tie(map_int_to_string& m, int i, const std::string& s) {
  map_int_to_string::iterator iter;
  bool did_insert;
  std::tie(iter, did_insert) = m.insert({i, s});

  if (did_insert) {
    std::cout << "insertion succeeded. ";
  } else {
    std::cout << "insertion failed. ";
  }

  std::cout << "key=" << iter->first << " value=" << iter->second << "\n";
}

int main(int argc, const char** argv) {
  std::unordered_map<int, std::string> mymap;
  mymap[1] = "one";

  try_insert_with_tie(mymap, 2, "two");
  try_insert_with_tie(mymap, 1, "one");

  return 0;
}
