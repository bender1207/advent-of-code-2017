#include <iostream>
#include <vector>

using namespace std;


int main()
{
  vector<char> values(0);

  char value;
  while (cin >> value) {
    values.push_back(value);
  }  

  double sum = 0;
  for (int i = 0; i < values.size(); ++i) {
    if (values[i] == values[(i+1) % values.size()]) {
      sum += values[i] - '0';
    }
  }

  cout << sum << endl;
  
  return 0;
}


