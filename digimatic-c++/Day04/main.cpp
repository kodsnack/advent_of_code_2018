// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <regex>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

//                  y     m    d    H     M  ID (-1 fall asleep, -2 wakes up, 0... id)
using event = tuple<int, int, int, int, int, int>;

event parseLine(const string& line)
{
	auto remaining = line;
	regex re1("\\[(\\d\\d\\d\\d)\\-(\\d\\d)\\-(\\d\\d) (\\d\\d):(\\d\\d)\\] ((falls asleep)|(wakes "
	          "up)|(Guard #(\\d+) begins shift))");
	smatch m;
	bool b = regex_match(line, m, re1);

	auto yr = stoi(m[1].str());
	auto mn = stoi(m[2].str());
	auto da = stoi(m[3].str());
	auto H = stoi(m[4].str());
	auto M = stoi(m[5].str());
	// if(H>20) H-=24;
	if(m[7].matched) // fall asleep
	{
		return make_tuple(yr, mn, da, H, M, -1);
	} else if(m[8].matched)
	{ // wakes up
		return make_tuple(yr, mn, da, H, M, -2);
	} else if(m[10].matched)
	{
		auto id = stoi(m[10].str());
		return make_tuple(yr, mn, da, H, M, id);
	}
	return event{};
}

vector<event> parseLines(const vector<string>& lines)
{
	vector<event> parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

auto value_selector = [](auto pair) { return pair.second; };

void solve_part1and2()
{
	auto events = parseLines(readLines(string(inputFile)));
	sort(events.begin(), events.end());

	//            id        Sum              M   count
	unordered_map<int, pair<int, unordered_map<int, int>>> stat;

	int cId = 0;
	int sleepStart = -1;
	int bestId = -1;
	for(auto& e : events)
	{
		auto id = get<5>(e);
		auto M = get<4>(e);
		if(id == -1)
		{
			sleepStart = M;
		} else if(id == -2)
		{
			if(sleepStart >= 0)
			{
				auto sleepEnd = M;
				auto& guardStat = stat[cId];
				auto& sum = guardStat.first;
				auto& minStat = guardStat.second;
				for(int i = sleepStart; i < sleepEnd; ++i)
				{
					if(minStat.find(i) == minStat.end())
					{
						minStat[i] = 0;
					}
					minStat[i]++;
					sum++;
				}
				if(bestId == -1)
					bestId = cId;
				else
				{
					if(sum > stat[bestId].first)
					{
						bestId = cId;
					}
				}
				sleepStart = -1;
			} else
			{
				assert(0);
			}
		} else
		{
			if(cId != id)
			{
				sleepStart = -1;
			}
			cId = id;
			if(stat.find(id) == stat.end())
			{
				stat[id] = {};
			}
		}
	}

	auto& minStat = stat[bestId].second;
	auto maxIt = max_element(minStat.begin(), minStat.end(),
	                         [](auto& p, auto& q) { return p.second < q.second; });
	auto maxM = maxIt->first;
	auto r = bestId * maxM;
	assert(r == 125444);
	cout << dayName << " - part 1: " << r << "" << endl;

	// part 2
	bestId = -1;
	int bestSleepSum = -1;
	int bestMinute = -1;
	for(auto& [gId, gStat] : stat)
	{
		auto& minStat = gStat.second;
		for(auto& [M, n] : minStat)
		{
			if(n > bestSleepSum)
			{
				bestSleepSum = n;
				bestMinute = M;
				bestId = gId;
			}
		}
	}
	auto r2 = bestId * bestMinute;
	assert(r2 == 18325);
	cout << dayName << " - part 2: " << r2 << endl;
}

int main()
{
	solve_part1and2();
	return 0;
}
