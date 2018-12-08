// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <memory>
#include <numeric>
#include <stack>
#include <string>
#include <utility>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

vector<int> parseLine(const string& line)
{
	auto remaining = line;
	size_t taken = 0;
	vector<int> numbers;
	while(remaining.length() > 0)
	{
		auto n = std::stoi(remaining, &taken);
		numbers.push_back(n);
		if((taken + 1) < remaining.length())
			remaining = remaining.substr(taken + 1);
		else
			remaining.clear();
	}
	return numbers;
}

struct Node
{
	int childCount{-1};
	int metaCount{-1};
	vector<shared_ptr<Node>> children; // reversed children
	vector<int> metas;
};

enum class Type
{
	Node,
	Meta
};
struct Job
{
	Job(Type type, shared_ptr<Node> node)
	    : type(type)
	    , node(node)
	{
	}
	Type type;
	shared_ptr<Node> node;
};

void readHead(const vector<int>& input, int& i, Node& n)
{
	n.childCount = input[i++];
	n.metaCount = input[i++];
}

void buildTree(const vector<int>& input, int& i, stack<Job>& q)
{
	while(!q.empty())
	{
		auto j = q.top();
		q.pop();

		if(j.type == Type::Node)
		{
			readHead(input, i, *(j.node));
			q.emplace(Type::Meta, j.node);
			for(int ci = 0; ci < j.node->childCount; ci++)
			{
				auto child = make_shared<Node>();
				j.node->children.push_back(child);
				q.emplace(Type::Node, child);
			}
		} else if(j.type == Type::Meta)
		{
			for(int ci = 0; ci < j.node->metaCount; ci++)
			{
				j.node->metas.push_back(input[i++]);
			}
		}
	}
}

void traverse(shared_ptr<Node> n, std::function<void(Node&)> f)
{
	f(*n);
	for(int i = n->childCount - 1; i >= 0; --i)
	{
		traverse(n->children[i], f);
	}
}

shared_ptr<Node> readAndBuildTree()
{
	auto input = parseLine(readLines(string(inputFile))[0]);
	int i = 0;
	stack<Job> q;
	auto root = make_shared<Node>();
	q.emplace(Type::Node, root);
	buildTree(input, i, q);
	return root;
}

void solve_part1()
{
	auto root = readAndBuildTree();
	int metaSum = 0;
	traverse(root,
	         [&metaSum](Node& n) { metaSum = accumulate(begin(n.metas), end(n.metas), metaSum); });

	assert(metaSum==40036);
	cout << dayName << " - part 1: " << metaSum << endl;
}

int calcNodeValue(shared_ptr<Node> n)
{
	if(n->childCount == 0)
	{
		return accumulate(begin(n->metas), end(n->metas), 0);
	} else
	{
		int childSum = 0;
		for(auto m : n->metas)
		{
			int i = m - 1;
			if(i >= 0 && i < n->childCount)
			{
				childSum += calcNodeValue(n->children[n->childCount - 1 - i]);
			}
		}
		return childSum;
	}
}

void solve_part2()
{
	auto root = readAndBuildTree();
	auto n = calcNodeValue(root);

	assert(n==21677);
	cout << dayName << " - part 2: " << n << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
