#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <sstream>
#include <algorithm>
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


vector<char> getChars(string word)
{
  vector<char> chars(0);

  for (char& c : word) {
    chars.push_back(c);
  }

  return chars;
}


bool isAnagram(string word1, string word2)
{
  if (word1 == word2) {
    return true;
  }

  if (word1.size() != word2.size()) {
    return false;
  }
  
  // word1 and word2 are different but have same size
  vector<char> chars2 = getChars(word2);

  for (char& c : word1) {
    vector<char>::iterator pos = find(chars2.begin(), chars2.end(), c);
    if (pos == chars2.end()) {
      return false;
    }

    // remove character from chars2 
    chars2.erase(pos);
  }

  return true;
}


bool isValid(string row)
{
  vector<string> words = getWords(row);
  
  for (int i = 0; i < words.size() - 1; ++i) {
    for (int j = i + 1; j < words.size(); ++j) {
      if (isAnagram(words[i], words[j])) {
	return false;
      }
    }
  }

  return true;
}


int main()
{
  //--------------------
  // Tests
  {
    assert(isValid("abcde fghij") == true);
    assert(isValid("abcde xyz ecdab") == false);
    assert(isValid("a ab abc abd abf abj") == true);
    assert(isValid("iiii oiii ooii oooi oooo") == true);
    assert(isValid("oiii ioii iioi iiio") == false);
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

