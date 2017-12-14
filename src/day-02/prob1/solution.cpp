#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <climits>

using namespace std;

int main()
{
  int checksum = 0;

  string row;
  while (getline(cin, row)) {
    stringstream stream(row);
 
    int min = INT_MAX;
    int max = INT_MIN;

    while (true) {
      int val;
      stream >> val;
      
      if (! stream) {
	break;
      }

      if (val < min) { min = val; }
      if (val > max) { max = val; }
    }

    checksum += max - min;
  }  

  cout << checksum << endl;
  
  return 0;
}
