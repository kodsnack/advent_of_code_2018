// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>
#include <iostream>
#include <stdexcept>
#include <string>

#include <algorithm>
#include <functional>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

using point = tuple<int, int>;

namespace std
{
	template <> struct hash<point>
	{
		size_t operator()(const point& p) const noexcept
		{
			auto h1 = get<0>(p);
			auto h2 = get<1>(p);
			return h1 * 121 + h2;
		}
	};
} // namespace std

bool operator<(point p, point q) noexcept
{
	if(get<0>(p) < get<0>(q))
		return true;
	if(get<0>(p) == get<0>(q))
	{
		if(get<1>(p) < get<1>(q))
			return true;
		return false;
	}
	return false;
}

using size2d = tuple<int, int>;
using rect = tuple<point, size2d>;
using canvas = unordered_map<point, int>;
canvas area;

rect parseLine(const string& line)
{
	auto remaining = line;
	remaining = remaining.substr(1);
	size_t taken = 0;
	auto n = std::stoi(remaining, &taken);
	remaining = remaining.substr(taken + 3);
	auto x = std::stoi(remaining, &taken);
	remaining = remaining.substr(taken + 1);
	auto y = std::stoi(remaining, &taken);
	remaining = remaining.substr(taken + 2);
	auto w = std::stoi(remaining, &taken);
	remaining = remaining.substr(taken + 1);
	auto h = std::stoi(remaining, &taken);

	return make_tuple(make_tuple(x, y), make_tuple(w, h));
}

vector<rect> parseLines(const vector<string>& lines)
{
	vector<rect> rects;
	for(auto& line : lines)
	{
		rects.push_back(parseLine(line));
	}
	return rects;
}

bool paintPoint(canvas& c, point p)
{
	auto it = c.find(p);
	if(it == c.end())
	{
		c.insert(make_pair(p, 1));
		return false;
	} else
	{
		it->second += 1;
		if(it->second > 2)
			return true;
		return false;
	}
}

bool paintRect(canvas& c, rect r)
{
	bool anyOverlap = false;
	point p = get<0>(r);
	size2d s = get<1>(r);
	for(int y = 0; y < get<1>(s); y++)
		for(int x = 0; x < get<0>(s); x++)
		{
			bool ov = paintPoint(c, make_tuple(get<0>(p) + x, get<1>(p) + y));
			if(ov)
				anyOverlap = true;
		}
	return anyOverlap;
}

void solve_part1()
{
	canvas c;
	auto rects = parseLines(readLines(string(inputFile)));
	for(auto r : rects)
	{
		paintRect(c, r);
	}
	int total = 0;
	for(auto [p, count] : c)
	{
		if(count > 1)
		{
			total++;
		}
	}
	cout << dayName << " - part 1: " << total << endl;
}

void solve_part2()
{
	unordered_set<int> nonOverlappedRects;
	canvas c;
	auto rects = parseLines(readLines(string(inputFile)));
	int id = 1;
	for(auto r : rects)
	{
		paintRect(c, r);
		nonOverlappedRects.insert(id);
		id++;
	}

	id = 1;
	bool found = false;
	for(auto r : rects)
	{
		bool anyOverlap = paintRect(c, r);
		if(!anyOverlap)
		{
			found = true;
			break;
		}
		id++;
	}

	if(found)
	{
		cout << dayName << " - part 2: " << id << endl;
	} else
	{
		cout << dayName << " - part 2: no solution found" << endl;
	}
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
