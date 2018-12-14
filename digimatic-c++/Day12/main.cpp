// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>

using namespace westerstrom;
using namespace std;

using Plants = string;
using Rules = unordered_map<string, char>;
using State = Plants;

pair<State, Rules> parseLines(const vector<string>& lines)
{
	pair<State, Rules> r;

	auto s = lines[0].substr(15);
	r.first = s;

	for(int i = 2; i < lines.size(); ++i)
	{
		auto& l = lines[i];
		auto llcrr = l.substr(0, 5);
		r.second[llcrr] = l[9];
	}
	return r;
}

auto computeSum(const string& state, int zeroOffset)
{
	int64_t sum = 0;
	for(int i = 0; i < state.length(); ++i)
	{
		if(state[i] == '#')
			sum += (i - zeroOffset);
	}
	return sum;
}

tuple<State, int, int> update(State s0, int zeroOffset, Rules& rules)
{
	s0 = "...." + s0 + "....";
	zeroOffset += 4;

	int len = static_cast<int>(s0.length());
	string s1 = "..";
	for(int i = 2; i < len - 2; i++)
	{
		auto sub = s0.substr(i - 2, 5);
		assert(rules.find(sub) != end(rules));
		s1 += rules[sub];
	}

	// trim
	auto f = s1.find('#');
	auto l = s1.rfind('#');
	s1 = s1.substr(f, 1 + l - f);
	zeroOffset -= static_cast<int>(f);
	auto sum = computeSum(s1, zeroOffset);

	return make_tuple(s1, zeroOffset, sum);
}

void solve_part1()
{
	auto [state, rules] = parseLines(readLines(string(inputFile) + ""s));
	int zeroOffset = 0;
	int64_t sum = 0;
	const int64_t n = 50000000000LL;
	int64_t finalSum = 0;
	int sameCount = 0;
	int sumDiff = 0;
	for(int64_t i = 1; i <= n; ++i)
	{
		auto [state1, zeroOffset1, sum1] = update(state, zeroOffset, rules);
		if(i == 20)
		{
			assert(sum1==3915);
			cout << dayName << " - part 1: " << sum1 << endl;
		}
		auto sumDiff1 = sum1 - sum;
		if(sumDiff1 == sumDiff)
			sameCount++;
		else
			sameCount = 0;

		if(sameCount > 4)
		{
			auto zeroOffsetDiff = zeroOffset - zeroOffset1;
			finalSum = sum1 + (n - i) * sumDiff1 * zeroOffsetDiff;
			break;
		}
		state = state1;
		zeroOffset = zeroOffset1;
		sum = sum1;
		sumDiff = sumDiff1;
	}
	assert(finalSum == 4900000001793);
	cout << dayName << " - part 2: " << finalSum << endl;
}

int main()
{
	solve_part1();
	return 0;
}
