// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <optional>

using namespace westerstrom;
using namespace std;

auto reactPolymer(string polymer)
{
	auto len = polymer.length();
	for(int i = 0; i < len - 1;)
	{
		char x = polymer[i];
		char y = polymer[i + 1];
		if(tolower(x) == tolower(y) && (((isupper(x) && islower(y)) || (islower(x) && isupper(y)))))
		{
			polymer.erase(i, 2);
			len -= 2;
			if(i > 0)
				i--;
		} else
		{
			++i;
		}
	}
	return len;
}

void solve_part1()
{
	auto parsedInput = readLines(string(inputFile));
	auto polymer = parsedInput[0];
	auto len = reactPolymer(polymer);
	assert(len == 11754);
	cout << dayName << " - part 1: " << len << endl;
}

void solve_part2()
{
	auto parsedInput = readLines(string(inputFile));
	auto polymer = parsedInput[0];
	optional<size_t> shortestLen;
	for(char c = 'a'; c <= 'z'; ++c)
	{
		auto p = polymer;
		p.erase(remove_if(p.begin(), p.end(), [c](char x) { return tolower(x) == tolower(c); }),
		        p.end());
		auto len = reactPolymer(p);
		if(!shortestLen)
		{
			shortestLen = len;
		} else
		{
			shortestLen = std::min(*shortestLen, len);
		}
	}
	assert(shortestLen && *shortestLen == 4098);
	cout << dayName << " - part 2: " << *shortestLen << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
