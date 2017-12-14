#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <climits>
#include <assert.h>

using namespace std;

int countJumps(vector<int> jumps)
{
  int index = 0;
  int counter = 0;

  while (index >= 0 && index < jumps.size()) {
    int offset = jumps[index];
    int increase = offset > 2 ? -1 : 1;
    jumps[index] += increase;
    index += offset;
    counter++;
  }

  return counter;
}


int main()
{
  //-- Tests -------------
  {
    static const int sample[] = {0, 3, 0, 1, -3};
    vector<int> jumps(sample, sample + sizeof(sample) / sizeof(sample[0]));
    assert(countJumps(jumps) == 10);
  }

  vector<int> jumps(0);
  int jump;
  while(cin >> jump) {
    jumps.push_back(jump);
  }

  cout << countJumps(jumps) << endl;

  return 0;
}
