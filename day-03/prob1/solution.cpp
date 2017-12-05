#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <climits>
#include <assert.h>

using namespace std;

int getDistanceTo(int square)
{
  int distance = 0;

  int i = 1;
  while (i * i < square) {
    i = i + 2;
  }

  if (i > 1) {
    int start_index = (i - 2) * (i - 2) + 1;
    int border_length = 4 * (i - 1);
    int border_index = square - start_index;
    int distance_sideways = abs(border_index % (i - 1) - (i/2 - 1));
    distance = /*distance_inwards*/ i / 2 + distance_sideways;    
  }

  return distance;
}


int main()
{
  //-- Tests -----------
  {
    assert(getDistanceTo(1) == 0);
    assert(getDistanceTo(12) == 3);
    assert(getDistanceTo(23) == 2);
    assert(getDistanceTo(1024) == 31);
  }
  //--------------------

  int square;
  cin >> square;

  int distance = getDistanceTo(square);
  cout << distance << endl;

  return 0;
}
