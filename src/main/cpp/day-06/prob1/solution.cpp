#include <iostream>
#include <vector>
#include <set>
#include <string>
#include <sstream>
#include <climits>
#include <assert.h>

using namespace std;


string toString(const vector<int>* vec)
{
  stringstream ss;
  for (int v : *vec) {
    ss << v;
  }

  return ss.str();
}

int getMaxValueIndex(const vector<int>* vec)
{
  int max_val = INT_MIN;
  int index = -1;
  for (int i = 0; i < vec->size(); ++i) {
    if ((*vec)[i] > max_val) {
      max_val = (*vec)[i];
      index = i;
    }
  }

  return index;
}

void redistribute(vector<int>* memory_banks, int index)
{
  //cout << "Redistributing \"" << toString(memory_banks) << "\" from index " << index << endl;

  int blocks_to_redistribute = (*memory_banks)[index];
  int blocks_to_add_to_all_banks = blocks_to_redistribute / memory_banks->size();
  int remaining_blocks = blocks_to_redistribute % memory_banks->size();

  (*memory_banks)[index] = 0;

  if (blocks_to_add_to_all_banks > 0) {
    for (int i = 0; i < memory_banks->size(); ++i) {
      (*memory_banks)[i] += blocks_to_add_to_all_banks;
    }
  }

  if (remaining_blocks > 0) {
    for (int i = 0; i < remaining_blocks; ++i) {
      (*memory_banks)[(index + i + 1) % memory_banks->size()]++;
    }
  }

  //cout << "Rsult : \"" << toString(memory_banks) << "\"" << endl;  
}

int countRedistributions(vector<int>* memory_banks)
{
  set<string> visited_states;

  int counter = 0;
  do {
    visited_states.insert(toString(memory_banks));
    int max_value_index = getMaxValueIndex(memory_banks);
    redistribute(memory_banks, max_value_index);
    counter++;
  } while (visited_states.find(toString(memory_banks)) == visited_states.end());

  /*
  set<string>::iterator it;
  for (it = visited_states.begin(); it != visited_states.end(); ++it) {
    cout << "String in set : \"" << *it << "\"" << endl;
  }
  */

  return counter;
}


int main()
{
  //-- Tests -------------
  {
    static const int sample[] = {0, 2, 7, 0};
    vector<int> memory_banks(sample, sample + sizeof(sample) / sizeof(sample[0]));
    assert(countRedistributions(&memory_banks) == 5);
    //cout << "Tests cleared!" << endl;
  }

  int blocks;
  vector<int> memory_banks(0);
  while(cin >> blocks) {
    memory_banks.push_back(blocks);
  }

  cout << countRedistributions(&memory_banks) << endl;

  return 0;
}
