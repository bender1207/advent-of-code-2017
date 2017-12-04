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
 
    vector<int> values(0);

    while (true) {
      int val;
      stream >> val;
      
      if (! stream) {
	break;
      }

      values.push_back(val);
    }

    for (int i = 0; i < values.size() - 1; ++i) {
      for (int j = i + 1; j < values.size(); ++j) {
	int a = values[i];
	int b = values[j];
	if (a % b == 0) {
	  checksum += a / b;
	  break;
	}
	if (b % a == 0) {
	  checksum += b / a;
	  break;
	}
      }
    }
  }  

  cout << checksum << endl;
  
  return 0;
}
