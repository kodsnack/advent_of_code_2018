// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <list>
#include <numeric>
#include <regex>
#include <string>
#include <utility>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

struct Point
{
	int x{0};
	int y{0};
	int z{0};
	int w{0};
};

Point makePoint(int x, int y, int z, int w)
{
	return {x, y, z, w};
}

namespace std
{
	template <> struct hash<Point>
	{
		size_t operator()(const Point& p) const noexcept
		{
			auto h1 = std::hash<int>{}(p.x);
			auto h2 = std::hash<int>{}(p.y << 8);
			auto h3 = std::hash<int>{}(p.z << 16);
			auto h4 = std::hash<int>{}(p.w << 24);
			// Mainly for demonstration purposes, i.e. works but is overly simple
			// In the real world, use sth. like boost.hash_combine
			return h1 ^ h2 ^ h3 ^ h4;
		}
	};
} // namespace std

int distance(Point p, Point q)
{
	return abs(p.x - q.x) + abs(p.y - q.y) + abs(p.z - q.z) + abs(p.w - q.w);
}

using Cluster = list<Point>;
using Clusters = list<Cluster>;

Point parseLine(const string& line)
{
	auto remaining = line;
	auto v = splitNumbers(line, true);
	return makePoint(v[0], v[1], v[2], v[3]);
}

auto parseLines(const vector<string>& lines)
{
	Cluster parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

bool canMerge(const Cluster& c1, const Cluster& c2)
{
	for(auto& p1 : c1)
	{
		for(auto& p2 : c2)
		{
			if(distance(p1, p2) <= 3)
				return true;
		}
	}
	return false;
}

// move all elements in c2 to c1
void merge(Cluster& c1, Cluster& c2)
{
	while(!c2.empty())
	{
		c1.push_back(move(c2.front()));
		c2.pop_front();
	}
}

bool mergeClusters(Clusters& c)
{
	bool mergedSomething = false;
	for(auto it1 = begin(c); it1 != end(c); ++it1)
	{
		auto it2 = it1;
		++it2;
		for(; it2 != end(c);)
		{
			if(canMerge(*it1, *it2))
			{
				merge(*it1, *it2);
				it2 = c.erase(it2), mergedSomething = true;
			} else
			{
				++it2;
			}
		}
	}
	return mergedSomething;
}

void solve_part1()
{
	Clusters clusters;
	auto parsedInput = parseLines(readLines(string(inputFile) + ""s));
	for(auto x : parsedInput)
	{
		clusters.push_back({x});
	}
	while(mergeClusters(clusters))
		;

	auto n = clusters.size();

	cout << dayName << " - part 1: " << n << endl;
	assert(n == 399);
}

int main()
{
	solve_part1();
	return 0;
}
