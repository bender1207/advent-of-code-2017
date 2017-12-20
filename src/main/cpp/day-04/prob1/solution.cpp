#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <sstream>
#include <climits>
#include <assert.h>

using namespace std;


vector<string> getWords(string row)
{
    stringstream ss(row);    
    vector<string> words(0);
    
    while (true) {
      string word;
      ss >> word;
      
      if (! ss) {
	break;
      }

      words.push_back(word);
    }

    return words;
}


bool isValid(string row)
{
  vector<string> words = getWords(row);
  set <string, greater<string> > words_unique;
  
  vector<string>::iterator it;
  for(it = words.begin(); it != words.end(); ++it) {
    words_unique.insert(*it);
  }

  return words_unique.size() == words.size();
}


int main()
{
  //--------------------
  // Tests
  {
    assert(isValid("aa bb cc dd ee") == true);
    assert(isValid("aa bb cc dd aa") == false);
    assert(isValid("aa bb cc dd aaa") == true);
  }
  //--------------------

  int valid = 0;
  
  string row;
  while (getline(cin, row)) {
    valid += isValid(row) ? 1 : 0;    
  }

  cout << valid << endl;

  return 0;
}

