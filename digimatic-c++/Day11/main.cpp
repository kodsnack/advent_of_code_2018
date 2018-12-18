// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <cassert>
#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>

using namespace westerstrom;
using namespace std;

const int serialNumber = 4455; // input
const int N = 300;

int calcGridPowerLevel(int x, int y, int serialNumber)
{
	if(x < 1 || x > N || y < 1 || y > N)
		return 0;
	int rackId = x + 10;
	int powerLevel = rackId * y;
	powerLevel += serialNumber;
	powerLevel *= rackId;
	powerLevel = (powerLevel / 100) % 10;
	powerLevel -= 5;
	return powerLevel;
}

int calcGridAreaSum(int x, int y, int s, int serialNumber)
{
	int sum = 0;
	for(int oy = 0; oy < s; oy++)
	{
		for(int ox = 0; ox < s; ox++)
		{
			sum += calcGridPowerLevel(x + ox, y + oy, serialNumber);
		}
	}
	return sum;
}

// part 1
tuple<int, int> findMax(int serialNumber)
{
	optional<int> maxLevel;
	int xmax = 0, ymax = 0;
	for(int y = 1; y <= N; y++)
	{
		for(int x = 1; x <= N; x++)
		{
			int pl = calcGridAreaSum(x, y, 3, serialNumber);
			if(!maxLevel || pl > maxLevel)
			{
				xmax = x;
				ymax = y;
				maxLevel = pl;
			}
		}
	}
	return make_tuple(xmax, ymax);
}

// part 2
tuple<int, int, int> findMax2(int serialNumber)
{
	optional<int> maxLevel;
	int xmax = 0, ymax = 0;
	int sizemax = 0;
	for(int y = 1; y <= N; y++)
	{
		for(int x = 1; x <= N; x++)
		{
			int pl = 0;
			for(int s = 1; s <= 300; ++s)
			{
				for(int ox = 0; ox < s; ++ox)
				{
					pl += calcGridPowerLevel(x + ox, y + s - 1, serialNumber);
				}
				for(int oy = 0; oy < s - 1; ++oy)
				{
					pl += calcGridPowerLevel(x + s - 1, y + oy, serialNumber);
				}
				if(!maxLevel || pl > maxLevel)
				{
					xmax = x;
					ymax = y;
					sizemax = s;
					maxLevel = pl;
				}
			}
		}
	}
	return make_tuple(xmax, ymax, sizemax);
}

void solve_part1()
{
	auto [xmax, ymax] = findMax(serialNumber);
	assert(xmax == 21 && ymax == 54);
	cout << dayName << " - part 1: " << xmax << "," << ymax << endl;
}

void solve_part2()
{
	auto [xmax, ymax, sizemax] = findMax2(serialNumber);
	assert(xmax == 236 && ymax == 268 && sizemax == 11);
	cout << dayName << " - part 2: " << (xmax) << "," << (ymax) << "," << sizemax << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
