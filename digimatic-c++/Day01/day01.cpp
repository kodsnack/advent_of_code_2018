// Advent of Code 2018 - Day 1
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>
#include <iostream>
#include <string>
#include <unordered_set>
#include <stdexcept>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;


int parseLine(const string& line)
{
	auto remaining = line;
	size_t taken = 0;
	auto n = std::stoi(remaining, &taken);
	return n;
}

void solve_day1_part1()
{
	auto lines = readLines(INPUT_FILE);
	int currentFreq = 0;
	for(auto& line : lines)
	{
		auto freqChange = parseLine(line);
		currentFreq += freqChange;
	}

	
	cout << "Day 1 - part 1: " << currentFreq << endl;
}


int findFirstDuplicate()
{
	auto lines = readLines(INPUT_FILE);
	int currentFreq = 0;
	unordered_set<int> seenFrequences;
	while(true)
	{
		for(auto& line : lines)
		{
			auto freqChange = parseLine(line);
			currentFreq += freqChange;
			if(seenFrequences.find(currentFreq) != seenFrequences.end())
				return currentFreq;
			seenFrequences.insert(currentFreq);
		}
	}
	throw std::exception();
}

void solve_day1_part2()
{
	try
	{
		auto f = findFirstDuplicate();
		cout << "Day 1 - part 2: "<< f << endl;
	} catch(const std::exception&)
	{
		cout << "Day 1 - part 2: no solution found."<<  endl;
	}
}

int main()
{
	solve_day1_part1();
	solve_day1_part2();
	return 0;
}
