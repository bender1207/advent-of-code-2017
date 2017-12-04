#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <climits>

using namespace std;

int main()
{
  int square;

  cin >> square;

  int i = 1;
  while (i * i < square) {
    i = i + 2;
  }

  if (i == 1) {
    cout << 0 << endl;
  }
  else {
    int start_index = (i - 2) * (i - 2) + 1;
    int border_length = 4 * (i - 1);
    int border_index = square - start_index;

    int distance_sideways = abs(border_index % (i - 1) - (i/2 - 1));
    int distance = /*distance_inwards*/ i / 2 + distance_sideways;  
    
    cout << distance << endl;
  }

  return 0;
}
