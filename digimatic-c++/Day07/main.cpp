// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

using Dep = pair<char, char>;
using Deps = vector<Dep>;

Dep parseLine(const string& line)
{
	return make_pair(line[5], line[36]);
}

Deps parseLines(const vector<string>& lines)
{
	vector<Dep> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

auto getAlphabet(const Deps& deps)
{
	vector<char> alphabet;
	for(auto d : deps)
	{
		alphabet.push_back(d.first);
		alphabet.push_back(d.second);
	}
	sort(begin(alphabet), end(alphabet));
	alphabet.erase(unique(begin(alphabet), end(alphabet)), end(alphabet));
	return alphabet;
}

bool hasParent(const Deps& deps, char c)
{
	for(auto d : deps)
	{
		if(d.second == c)
			return true;
	}
	return false;
}

void solve_part1()
{
	auto deps = parseLines(readLines(string(inputFile)));
	auto alphabet = getAlphabet(deps);

	string r;
	while(alphabet.size() > 0)
	{
		char c;
		for(int i = 0; i < alphabet.size(); ++i)
		{
			char cand = alphabet[i];
			if(!hasParent(deps, cand))
			{
				c = cand;
				alphabet.erase(alphabet.begin() + i);
				break;
			}
		}
		r += c;
		deps.erase(remove_if(deps.begin(), deps.end(), [c](auto&& d) { return d.first == c; }),
		           deps.end());
	}
	assert(r == "GDHOSUXACIMRTPWNYJLEQFVZBK");
	cout << dayName << " - part 1: " << r << endl;
}

void solve_part2()
{
	auto deps = parseLines(readLines(string(inputFile)));
	auto alphabet = getAlphabet(deps);

	const int workerCount = 5;
	// Worker pairs of current letter beeing worked on or '\0' if worker is free, and finnish time.
	vector<pair<char, int>> worker;
	worker.resize(workerCount);

	int t = 0;
	string r;

	while(alphabet.size() > 0)
	{
		// job completion
		{
			auto firstCompleteIt = min_element(begin(worker), end(worker), [](auto&& x, auto&& y) {
				if(x.first == '\0')
				{
					// x == 0
					return false;
				} else if(y.first == '\0')
				{
					// x != 0 && y == 0
					return true;
				}
				return x.second < y.second;
			});
			if(firstCompleteIt->first != '\0')
			{
				char c = firstCompleteIt->first;
				t = firstCompleteIt->second;
				alphabet.erase(find(begin(alphabet), end(alphabet), c));
				deps.erase(
				    remove_if(deps.begin(), deps.end(), [c](auto&& d) { return d.first == c; }),
				    deps.end());
				firstCompleteIt->first = '\0';
				r += c;
			}
		}

		// job assigning
		for(int workerToAssign = 0; workerToAssign < workerCount; ++workerToAssign)
		{
			if(worker[workerToAssign].first != '\0')
				continue;

			// get next job
			char c = '\0';
			for(int i = 0; i < alphabet.size(); ++i)
			{
				char cand = alphabet[i];

				if(!hasParent(deps, cand))
				{
					if(find_if(worker.begin(), worker.end(),
					           [cand](auto&& w) { return w.first == cand; }) == worker.end())
					{
						c = cand;
						break;
					}
				}
			}

			if(c != '\0')
			{ // found a possible job to assign
				// assign job
				worker[workerToAssign].first = c;
				worker[workerToAssign].second = t + 61 + static_cast<int>(c - 'A');
			}
		}
	}

	assert(t == 1024);
	cout << dayName << " - part 2: " << t << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
