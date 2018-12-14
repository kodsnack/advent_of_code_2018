// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <numeric>
#include <string>
#include <unordered_set>
#include <utility>

using namespace westerstrom;
using namespace std;

using Point = pair<int, int>;
constexpr int gridSize = 400;

auto parseLine(const string& line)
{
	auto remaining = line;
	size_t taken = 0;
	auto x = std::stoi(remaining, &taken);
	remaining = remaining.substr(taken + 2);
	auto y = std::stoi(remaining, &taken);
	return make_pair(x, y);
}

auto parseLines(const vector<string>& lines)
{
	vector<Point> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

int getClosestPoint(Point p, const vector<Point>& points)
{
	vector<int> distances;
	distances.resize(points.size());
	transform(points.begin(), points.end(), distances.begin(),
	          [p](Point q) { return (abs(p.first - q.first)) + (abs(p.second - q.second)); });
	auto f1 = min_element(distances.begin(), distances.end());
	auto n = count(distances.begin(), distances.end(), (*f1));
	if(n > 1)
		return -1;
	return static_cast<int>(distance(distances.begin(), f1));
}

void solve_part1()
{
	auto points = parseLines(readLines(string(inputFile)));
	vector<pair<int, int>> areas;
	areas.resize(points.size(), make_pair(-1, 0));
	unordered_set<int> banned;
	for(int y = 0; y < gridSize; ++y)
	{
		for(int x = 0; x < gridSize; ++x)
		{
			int p = getClosestPoint(make_pair(x, y), points);
			if(p >= 0)
			{
				//			g[y][x] = p;
				areas[p].first = p;
				areas[p].second += 1;
				if(y == 0 || x == 0 || y == (gridSize - 1) || x == (gridSize - 1))
				{
					banned.insert(p);
				}
			}
		}
	}
	sort(areas.begin(), areas.end(), [](auto&& p, auto&& q) { return p.second > q.second; });
	int r{-1};
	for(auto a : areas)
	{
		if(a.first != -1 && banned.find(a.first) == banned.end())
		{
			r = a.second;
			break;
		}
	}
	assert(r != -1);
	assert(r == 3251);
	cout << dayName << " - part 1: " << r << endl;
}

int sumDistances(Point p, const vector<Point>& points)
{
	vector<int> distances;
	distances.resize(points.size());
	transform(points.begin(), points.end(), distances.begin(),
	          [p](Point q) { return (abs(p.first - q.first)) + (abs(p.second - q.second)); });
	return accumulate(distances.begin(), distances.end(), 0);
}

void solve_part2()
{
	//	Grid g;
	auto points = parseLines(readLines(string(inputFile)));
	int r = 0;
	for(int y = 0; y < gridSize; ++y)
	{
		for(int x = 0; x < gridSize; ++x)
		{
			int p = sumDistances(make_pair(x, y), points);
			if(p < 10000)
			{
				r++;
			}
		}
	}
	assert(r == 47841);
	cout << dayName << " - part 2: " << r << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
