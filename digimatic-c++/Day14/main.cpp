// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <deque>
#include <functional>
#include <iostream>
#include <list>
#include <string>
#include <utility>

using namespace westerstrom;
using namespace std;

template <typename ForwardIter>
ForwardIter stepForward(ForwardIter it, int n, const ForwardIter b, const ForwardIter e)
{
	for(int i = 0; i < n; i++)
	{
		++it;
		if(it == e)
			it = b;
	}
	return it;
}

template <typename IntVectorT, typename ForwardIter>
bool isDigitsMatch(const IntVectorT& digits, ForwardIter it, ForwardIter e)
{
	int n = static_cast<int>(digits.size());
	for(int i = 0; i < n; ++i)
	{
		if(it == e)
			return false;
		if(*it != digits[i])
			return false;
		it++;
	}
	return true;
}

auto getDigits(int v)
{
	deque<int> digits;
	while(true)
	{
		auto r = (v % 10);
		digits.push_front(r);
		if(v <= 9)
			break;
		v = v / 10;
	}
	return digits;
}

void solve(int input)
{
	auto inputDigits = getDigits(input);

	list<int> recipes{3, 7};
	auto it2 = recipes.begin();
	auto it1 = it2++;
	int numRecipes = 2;

	bool answerEmitted = false;
	int64_t searchIndex = 0;
	auto searchIt = recipes.begin();
	while(true)
	{
		auto v = (*it1) + (*it2);
		auto it = recipes.end();
		while(true)
		{
			auto r = (v % 10);
			it = recipes.insert(it, r);
			++numRecipes;
			if(v <= 9)
				break;
			v = v / 10;
		}
		it1 = stepForward(it1, 1 + (*it1), begin(recipes), end(recipes));
		it2 = stepForward(it2, 1 + (*it2), begin(recipes), end(recipes));

		if(!answerEmitted && (numRecipes > (input + 10)))
		{
			auto it = stepForward(begin(recipes), input, begin(recipes), end(recipes));
			int64_t sum = 0;
			for(int i = 0; i < 10; i++)
			{
				sum *= 10;
				sum += *it;
				++it;
			}
			cout << dayName << " - part 1: " << sum << endl; // 6985103122
			answerEmitted = true;
		}

		if(searchIndex + 10 < numRecipes)
		{
			bool m = isDigitsMatch(inputDigits, searchIt, end(recipes));
			if(m)
			{
				cout << dayName << " - part 2: " << searchIndex << endl; // 20182290
				return;
			}
			++searchIndex;
			++searchIt;
		}
	}

	assert(0);
}

int main()
{
	int input = 380621;
	solve(input);
	return 0;
}
