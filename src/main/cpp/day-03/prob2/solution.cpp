#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <climits>
#include <assert.h>

using namespace std;

struct Coord
{
  int x;
  int y;
};

Coord getXY(int square)
{
  if (square == 1) {
    Coord c = {0, 0};
    return c;
  }

  int i = 1;
  while (i * i < square) {
    i = i + 2;
  }

  int start_index = (i - 2) * (i - 2) + 1;  // one above bottom right
  int border_length = 4 * (i - 1);
  int border_index = square - start_index;

  int x;
  int y;
  
  if (border_index < (i - 1)) {
    // right side
    x = i/2;
    y = border_index % (i - 1) - (i/2 - 1);
  }
  else if (border_index < 2 * (i - 1)) {
    // top side
    y = i/2;
    x = -(border_index % (i - 1) - (i/2 - 1));
  }
  else if (border_index < 3 * (i - 1)) {
    // left side
    x = -i/2;
    y = -(border_index % (i - 1) - (i/2 - 1));
  }
  else {
    // bottom side
    y = -i/2;
    x = border_index % (i - 1) - (i/2 - 1);
  }
  
  Coord c = {x, y};
  return c;
}


int getSquare(int x, int y)
{
  int square = 1;

  Coord c = getXY(square);
  while (!(c.x == x && c.y == y)) {
    c = getXY(++square);
  }

  return square;
}


int sum(int square)
{
  if (square == 1) {
    return 1;
  }

  int s = 0;
  Coord c = getXY(square);

  for (int y = c.y - 1; y <= c.y + 1; ++y) {
    for (int x = c.x - 1; x <= c.x + 1; ++x) {
      if (x == c.x && y == c.y) {
	continue;
      }

      int neighbour = getSquare(x, y);

      bool neighbourLessThanSquare = neighbour < square;
      if (neighbourLessThanSquare) {
	s += sum(neighbour);
      }
    }
  }

  return s;
}


int getFirstSumGreaterThan(int value)
{
  int square = 1;
  int s = sum(square);

  while (s <= value) {
    square++;
    s = sum(square);
  }
  
  return s;
}


int main()
{
  //--------------------
  // Tests
  {
    // getXY()
    Coord c16 = getXY(16);
    assert(c16.x == -1 && c16.y == 2);

    Coord c26 = getXY(26);
    assert(c26.x == 3 && c26.y == -2);

    // getSquare()
    assert(getSquare(-1, 2) == 16);
    assert(getSquare(3, -2) == 26);

    // sum()
    assert(sum(1) == 1);
    assert(sum(2) == 1);
    assert(sum(3) == 2);
    assert(sum(4) == 4);
    assert(sum(5) == 5);
    assert(sum(6) == 10);  
    assert(sum(7) == 11);
    assert(sum(13) == 59);
    assert(sum(14) == 122);

    // getFirstSumGreaterThan()
    assert(getFirstSumGreaterThan(4) == 5);
    assert(getFirstSumGreaterThan(5) == 10);
    assert(getFirstSumGreaterThan(59) == 122);
  }
  //--------------------

  int value;
  cin >> value;

  int s = getFirstSumGreaterThan(value);

  cout << s << endl;

  return 0;
}

