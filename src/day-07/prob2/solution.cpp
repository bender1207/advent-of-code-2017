#include <iostream>
#include <algorithm>
#include <vector>
#include <set>
#include <map>
#include <string>
#include <sstream>
#include <climits>
#include <assert.h>

#define UNKNOWN -1

using namespace std;

struct Node
{
  string name;
  int weight;
  int tree_weight;

  Node* parent;
  vector<Node*> children;
};

//---------------------------------------------------------
// Debug method for printing a single node
//
//
string toString(const Node& node)
{
  stringstream ss;
  ss << node.name << " (w=" << node.weight << ", t=" << node.tree_weight << ")";

  if (!node.children.empty()) {
    ss << " ->";
    for (Node* child : node.children) {
      ss << " " << child->name;
    }
  }

  string parent = node.parent == NULL ? "NULL" : node.parent->name;
  ss << " (p=" << parent << ")";

  return ss.str();
}


//---------------------------------------------------------
// Debug method for printing all nodes in the node map
//
//
void printNodes(map<string, Node>& node_map)
{
  cout << "== Nodes ==========" << endl;
  map<string, Node>::iterator it;

  for (it = node_map.begin(); it != node_map.end(); ++it) {
    cout << toString(it->second) << endl;
  }
  cout << "===================" << endl;
}



//---------------------------------------------------------
// Utility method for trimming a string
//
//
string trim(const string& str)
{
    size_t first = str.find_first_not_of(' ');
    if (string::npos == first)
    {
        return str;
    }
    size_t last = str.find_last_not_of(' ');
    string sub = str.substr(first, (last - first + 1));
    return sub == " " ? "" : sub;
}


//---------------------------------------------------------
// Utility method for tokenizing a string
//
//
vector<string> tokenize(const string& str)
{
    stringstream ss(str);
    string s;
    vector<string> vec(0);

    while (getline(ss, s, ' ')) {
      string st = trim(s);
      if (!st.empty()) {
	vec.push_back(st);
      }
    }
  
    return vec;
}


//---------------------------------------------------------
// Parse a weight token
//
//
int parseWeightToken(const string& weight_token)
{
  // remove '(' and ')'
  int weight = stoi(weight_token.substr(1, weight_token.size()-2));
  return weight;
}


//---------------------------------------------------------
// Recursive method for getting the tree weight of a node.
// The tree weight is the weight of the node plus the weight
// of all its children.
//
// Also writes the correct weight sought to the
// corrected_weight input parameter.
//
//
int getTreeWeight(Node& node, /*OUTPUT*/ vector<int>& corrected_weight)
{
  // Has tree weight already been calculated
  if (node.tree_weight != UNKNOWN) {
    return node.tree_weight;
  }

  // Compute tree weight
  node.tree_weight = node.weight;
    
  if (! node.children.empty()) {

    vector<int> child_tree_weights(0);
    for (Node* child : node.children) {

      int child_tree_weight = getTreeWeight(*child, corrected_weight);
      node.tree_weight += child_tree_weight;

      child_tree_weights.push_back(child_tree_weight);
    }

    int min = *min_element(child_tree_weights.begin(), child_tree_weights.end());
    int max = *max_element(child_tree_weights.begin(), child_tree_weights.end());
    
    if (corrected_weight[0] == UNKNOWN && min != max) {
      // weight discrepancy found
      int num_min = count(child_tree_weights.begin(), child_tree_weights.end(), min);
      int num_max = count(child_tree_weights.begin(), child_tree_weights.end(), max);

      int diff = max - min;

      int faulty_weight = num_min < num_max ? min : max;
      for (Node* child : node.children) {
	if (child->tree_weight == faulty_weight) {
	  int weight = child->weight;
	  corrected_weight[0] = num_min < num_max ? weight + diff : weight - diff;
	}
      }
    }
  }

  return node.tree_weight;
}


//---------------------------------------------------------
// Recursive method for finding the root node.
//
//
Node getRootNode(Node& node)
{
  if (node.parent == NULL) {
    return node;
  }

  return getRootNode(*(node.parent));
}


//---------------------------------------------------------
// Core method
//
//
int getCorrectedWeight(vector<string> rows)
{
  // Create nodes
  //
  map<string, Node> node_map;

  for (string row : rows) {
    vector<string> row_tokens = tokenize(row);
    string name = row_tokens[0];
    int weight = parseWeightToken(row_tokens[1]);
    
    vector<Node*> children;
    Node node = {name, weight, UNKNOWN, NULL, children};

    node_map.insert(pair<string, Node>(name, node));
  }


  // Connect nodes
  //
  for (string row : rows) {
    vector<string> row_tokens = tokenize(row);
    string name = row_tokens[0];

    map<string, Node>::iterator it = node_map.find(name);
    Node* node = &(it->second);

    for (int i = 3; i < row_tokens.size(); ++i) {
      string token = row_tokens[i];
      // strip trailing comma
      string child_name = i == row_tokens.size() - 1 ? token : token.substr(0, token.size() - 1);

      map<string, Node>::iterator cit = node_map.find(child_name);
      Node* child = &(cit->second);

      node->children.push_back(child);
      child->parent = node;
    }
  }


  // Get root node
  //
  Node root = getRootNode(node_map.begin()->second);


  // Compute tree weights and find corrected weight 
  //
  vector<int> corrected_weight;
  corrected_weight.push_back(UNKNOWN);

  int tree_weight = getTreeWeight(root, corrected_weight);

  //printNodes(node_map);

  return corrected_weight[0];
}




int main()
{
  //-- Tests -------------
  {
    vector<string> tokens = tokenize("hej hopp    tjohoo");
    assert(tokens[0] == "hej");
    assert(tokens[1] == "hopp");
    assert(tokens[2] == "tjohoo");

    assert(parseWeightToken("(5)") == 5);
    assert(parseWeightToken("(21)") == 21);
    assert(parseWeightToken("(556)") == 556);
    
    vector<string> rows(0);
    rows.push_back("pbga (66)");
    rows.push_back("xhth (57)");
    rows.push_back("ebii (61)");
    rows.push_back("havc (66)");
    rows.push_back("ktlj (57)");
    rows.push_back("fwft (72) -> ktlj, cntj, xhth");
    rows.push_back("qoyq (66)");
    rows.push_back("padx (45) -> pbga, havc, qoyq");
    rows.push_back("tknk (41) -> ugml, padx, fwft");
    rows.push_back("jptl (61)");
    rows.push_back("ugml (68) -> gyxo, ebii, jptl");
    rows.push_back("gyxo (61)");
    rows.push_back("cntj (57)");

    assert(getCorrectedWeight(rows) == 60);
  }
  //----------------------

  string row;
  vector<string> rows(0);
  while (getline(cin, row)) {
    rows.push_back(row);    
  }

  cout << getCorrectedWeight(rows) << endl;

  return 0;
}
