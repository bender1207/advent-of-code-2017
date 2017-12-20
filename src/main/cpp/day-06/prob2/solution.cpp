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

  //cout << "Result : \"" << toString(memory_banks) << "\"" << endl;  
}

int countRedistributionLoopSize(vector<int>* memory_banks)
{
  set<string> visited_states;
  vector<string> visited_states_in_order(0);

  do {
    string memory_banks_as_string = toString(memory_banks);
    visited_states.insert(memory_banks_as_string);
    visited_states_in_order.push_back(memory_banks_as_string);

    int max_value_index = getMaxValueIndex(memory_banks);
    redistribute(memory_banks, max_value_index);
  } while (visited_states.find(toString(memory_banks)) == visited_states.end());

  vector<string>::iterator it;
  string str = toString(memory_banks);
  int loop_size = 0;
  for (it = find(visited_states_in_order.begin(), visited_states_in_order.end(), str);
       it != visited_states_in_order.end();
       ++it, ++loop_size) {}

  return loop_size;
}


int main()
{
  //-- Tests -------------
  {
    static const int sample[] = {0, 2, 7, 0};
    vector<int> memory_banks(sample, sample + sizeof(sample) / sizeof(sample[0]));
    assert(countRedistributionLoopSize(&memory_banks) == 4);
    //cout << "Tests cleared!" << endl;
  }

  int blocks;
  vector<int> memory_banks(0);
  while(cin >> blocks) {
    memory_banks.push_back(blocks);
  }

  cout << countRedistributionLoopSize(&memory_banks) << endl;

  return 0;
}
