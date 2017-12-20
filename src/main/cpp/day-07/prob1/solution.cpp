#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <string>
#include <sstream>
#include <climits>
#include <assert.h>

using namespace std;


//-------------------------------------------------------------
// Utility method for trimming a string.
//
//
string trim(const string& str)
{
    size_t first = str.find_first_not_of(' ');
    if (string::npos == first)
    {
        return str;
    }
    size_t last = str.find_last_not_of(' ');
    string sub = str.substr(first, (last - first + 1));
    return sub == " " ? "" : sub;
}


//-------------------------------------------------------------
// Utility method for tokenizing a string.
//
//
vector<string> tokenize(const string& str)
{
    stringstream ss(str);
    string s;
    vector<string> vec(0);

    while (getline(ss, s, ' ')) {
      string st = trim(s);
      if (!st.empty()) {
	vec.push_back(st);
      }
    }
  
    return vec;
}


//-------------------------------------------------------------
// Parse a weight token.
//
//
int parseWeightToken(const string& weight_token)
{
  // remove '(' and ')'
  int weight = stoi(weight_token.substr(1, weight_token.size()-2));
  return weight;
}


//-------------------------------------------------------------
// Recursive method for finding the bottom program.
//
//
string findBottomProgramRec(string& program, map<string, string>& parent_map)
{
  map<string, string>::iterator it = parent_map.find(program);
  if (it == parent_map.end()) {
    return program;
  }

  return findBottomProgramRec(it->second, parent_map);
}


//-------------------------------------------------------------
// Core method.
//
//
string getBottomProgram(vector<string> rows)
{
  map<string, string> parent_map;

  for (string row : rows) {
    // Put program in set
    vector<string> row_tokens = tokenize(row);
    string program_name = row_tokens[0];
    int weight = parseWeightToken(row_tokens[1]);

    // Store children in parent map
    for (int i = 3; i < row_tokens.size(); ++i) {
      string token = row_tokens[i];
      // strip trailing comma
      string held_program = i == row_tokens.size() - 1 ? token : token.substr(0, token.size() - 1);

      parent_map.insert(pair<string, string>(held_program, program_name));
    }    
  }

  string program = parent_map.begin()->first;
  return findBottomProgramRec(program, parent_map);  
}


int main()
{
  //-- Tests -------------
  {
    vector<string> tokens = tokenize("hej hopp    tjohoo");
    assert(tokens[0] == "hej");
    assert(tokens[1] == "hopp");
    assert(tokens[2] == "tjohoo");

    assert(parseWeightToken("(5)") == 5);
    assert(parseWeightToken("(21)") == 21);
    assert(parseWeightToken("(556)") == 556);
    
    vector<string> rows(0);
    rows.push_back("pbga (66)");
    rows.push_back("xhth (57)");
    rows.push_back("ebii (61)");
    rows.push_back("havc (66)");
    rows.push_back("ktlj (57)");
    rows.push_back("fwft (72) -> ktlj, cntj, xhth");
    rows.push_back("qoyq (66)");
    rows.push_back("padx (45) -> pbga, havc, qoyq");
    rows.push_back("tknk (41) -> ugml, padx, fwft");
    rows.push_back("jptl (61)");
    rows.push_back("ugml (68) -> gyxo, ebii, jptl");
    rows.push_back("gyxo (61)");
    rows.push_back("cntj (57)");

    assert(getBottomProgram(rows) == "tknk");
  }
  //----------------------

  string row;
  vector<string> rows(0);
  while (getline(cin, row)) {
    rows.push_back(row);    
  }

  cout << getBottomProgram(rows) << endl;

  return 0;
}
