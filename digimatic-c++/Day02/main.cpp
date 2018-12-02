// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <algorithm>
#include <cassert>
#include <common/common.h>
#include <iostream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

void solve_part1()
{
	auto boxes = readLines(string(inputFile));
	int numPairs{0}, numTripples{0};
	for(auto box : boxes)
	{
		unordered_map<char, int> charCounts;
		for(char c : box)
		{
			if(charCounts.find(c) == charCounts.end())
			{
				charCounts.insert(make_pair(c, 1));
			} else
			{
				charCounts[c] = charCounts[c] + 1;
			}
		}
		for(auto [c, count] : charCounts)
		{
			if(count == 2)
			{
				numPairs++;
				break;
			}
		}
		for(auto [c, count] : charCounts)
		{
			if(count == 3)
			{
				numTripples++;
				break;
			}
		}
	}
	auto checksum = numPairs * numTripples;

	cout << dayName << " - part 1: " << checksum << endl;
}

string findBoxID(const vector<string> boxes)
{
	vector<unordered_set<string>> s;
	s.resize(boxes[0].length()); // assume all box strings is same length
	for(auto box : boxes)
	{
		for(int i = 0; i < box.length(); i++)
		{
			auto b = box;
			b.erase(b.begin() + i);
			if(s[i].find(b) != s[i].end())
			{
				// yes
				return b;
			}
			s[i].insert(b);
		}
	}
	throw exception();
}

void solve_part2()
{
	auto boxes = readLines(string(inputFile));
	cout << dayName << " - part 2: ";
	try
	{
		auto id = findBoxID(boxes);
		cout << id << endl;
	} catch(const exception&)
	{
		cout << endl;
	}
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
