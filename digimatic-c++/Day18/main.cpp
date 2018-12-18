// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <string>
#include <utility>

using namespace westerstrom;
using namespace std;

using Map = vector<string>;

void update(const Map& m, Map& m2) noexcept
{
	auto w = m[0].size();
	auto h = m.size();
	for(int y = 0; y < h; y++)
	{
		for(int x = 0; x < w; x++)
		{
			char a = m[y][x];
			int treeCount = 0;
			int lumberYardCount = 0;
			for(int yi = -1; yi <= 1; yi++)
			{
				for(int xi = -1; xi <= 1; xi++)
				{
					if((yi == 0 && xi == 0) || (x + xi) < 0 || (x + xi) >= w || (y + yi) < 0 ||
					   (y + yi) >= h)
						continue;
					char c = m[y + yi][x + xi];
					if(c == '|')
						treeCount++;
					else if(c == '#')
						lumberYardCount++;
				}
			}

			if(a == '.' && treeCount >= 3)
			{
				m2[y][x] = '|';
			} else if(a == '|' && lumberYardCount >= 3)
			{
				m2[y][x] = '#';
			} else if(a == '#')
			{
				if(lumberYardCount >= 1 && treeCount >= 1)
					m2[y][x] = '#';
				else
					m2[y][x] = '.';
			} else
			{
				m2[y][x] = m[y][x];
			}
		}
	}
}

void printMap(const Map& m)
{
	for(auto& l : m)
	{
		cout << l << "\n";
	}
	cout << endl;
}

pair<int, int> countTreesAndLumberyard(const Map& m) noexcept
{
	int treeCount = 0;
	int lumberYardCount = 0;
	auto w = m[0].size();
	auto h = m.size();
	for(int y = 0; y < h; y++)
	{
		for(int x = 0; x < w; x++)
		{
			char c = m[y][x];
			if(c == '|')
				treeCount++;
			else if(c == '#')
				lumberYardCount++;
		}
	}
	return make_pair(treeCount, lumberYardCount);
}
int calcResourceValue(const Map& m) noexcept
{
	auto [treeCount, lumberYardCount] = countTreesAndLumberyard(m);
	return treeCount * lumberYardCount;
}

void solve_part1()
{
	auto m = readLines(string(inputFile) + ""s);
	auto m2 = m;
	for(int i = 0; i < 10 / 2; i++)
	{
		update(m, m2); // m2 <- m
		update(m2, m); // m <- m2
	}
	auto [treeCount, lumberYardCount] = countTreesAndLumberyard(m);
	assert((lumberYardCount * treeCount) == 588436);
	cout << dayName << " - part 1: " << (lumberYardCount * treeCount) << endl;
}

void solve_part2()
{
	auto m = readLines(string(inputFile));
	auto m2 = m;

	int i = 0;
	int n = 1000;
	for(; i < n; i += 2)
	{
		update(m, m2); // m2 <- m
		update(m2, m); // m <- m2
	}
	vector<int> values;
	while(true)
	{
		update(m, m2); // m2 <- m
		i++;
		int v = calcResourceValue(m2);
		if(values.size() == 0 || v != values[0])
			values.push_back(v);
		else
			break;
		swap(m, m2);
	}
	int cycleLength = static_cast<int>(values.size());
	int index = (1000000000 - n - 1) % cycleLength;
	auto v = values[index];
	cout << dayName << " - part 2: " << v << endl;
	assert(v == 195290);
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
