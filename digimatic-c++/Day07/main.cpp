// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <deque>
#include <functional>
#include <iostream>
#include <regex>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <map>

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

//using DepTree = multimap<char, char>;

/*auto findNext(const Deps& deps)
{
	unordered_map<char, int> counts;
	for(int i=0;i<deps.size();i++)
	{
		counts[deps[i].second] ++;
	}
	vector<pair<char, int>> countsv;
	countsv.resize(counts.size());
	copy(counts.begin(), counts.end(), countsv.begin());

	countsv.erase(remove_if(countsv.begin(), countsv.end(), 
		[](const pair<char, int>& x) { return x.second > 0; }), countsv.end());
	char best = 0;
	auto it = min_element(countsv.begin(), countsv.end(), [](auto&& x, auto&& y) {
		return x.first < y.first;
	});
	if(it != countsv.end())
		return it->first;
	else
		return '\0';
}*/

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

	vector<char> alphabet;
	for(auto d : deps)
	{
		alphabet.push_back(d.first);
		alphabet.push_back(d.second);
	}
	sort(begin(alphabet), end(alphabet));
	alphabet.erase(unique(begin(alphabet), end(alphabet)), end(alphabet));

	string r;
	while(alphabet.size() > 0)
	{
		char c;
		for(int i=0;i<alphabet.size();++i)
		{
			char cand = alphabet[i];
			if(!hasParent(deps, cand))
			{
				c = cand;
				alphabet.erase(alphabet.begin()+i);
				break;
			}
		}


		r += c;

		deps.erase(remove_if(deps.begin(), deps.end(), [c](auto&& d) {return d.first == c; }), deps.end());
	}


	cout << dayName << " - part 1: " << r << endl;
}

void solve_part2()
{
	auto deps = parseLines(readLines(string(inputFile)));

	vector<char> alphabet;
	for(auto d : deps)
	{
		alphabet.push_back(d.first);
		alphabet.push_back(d.second);
	}
	sort(begin(alphabet), end(alphabet));
	alphabet.erase(unique(begin(alphabet), end(alphabet)), end(alphabet));

	int t=0;

	tuple<char, int> worker[2] = { {'\0', 0}, {'\0', 0}};

	string r;
	while(alphabet.size() > 0)
	{
		vector<char> c;
		for(int i=0;i<alphabet.size();++i)
		{
			char cand = alphabet[i];
			if(!hasParent(deps, cand))
			{
				c.push_back(cand);
				alphabet.erase(alphabet.begin()+i);
				if(c.size()==2)
					break;
			}
		}

//		r += c;

//		deps.erase(remove_if(deps.begin(), deps.end(), [c](auto&& d) {return d.first == c; }), deps.end());
	}


	cout << dayName << " - part 2: " << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
