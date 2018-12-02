// Advent of Code 2018
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

vector<int> parseLines(const vector<string>& lines)
{
	vector<int> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

void solve_part1()
{
	auto parsedInput = parseLines(readLines(string(inputFile)));
	for(auto x : parsedInput)
	{
		
	}
	cout << dayName << " - part 1: " << "" << endl;
}



void solve_part2()
{
	cout << dayName << " - part 2: " << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
