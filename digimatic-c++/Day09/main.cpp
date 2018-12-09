// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <list>
#include <stdexcept>
#include <string>
#include <utility>

using namespace westerstrom;
using namespace std;

// input
const int numPlayers = 405;
const int numMarbles = 71700;

template <typename It> It stepForward(It b, It e, It it, int n)
{
	for(int i = 0; i < n; i++)
	{
		if(it == e)
			it = b;
		++it;
	}
	return it;
}

template <typename It> It stepBack(It b, It e, It it, int n)
{
	for(int i = 0; i < n; i++)
	{
		if(it == b)
			it = e;
		--it;
	}
	return it;
}

pair<int, uint64_t> playMarble(int numPlayers, int numMarbles)
{
	list<int> board(1, 0);
	auto currentBoardIt = board.begin();
	int currentPlayer = 0;
	vector<uint64_t> scores(numPlayers, 0);

	for(int currentMarble = 1; currentMarble <= numMarbles; ++currentMarble)
	{
		if((currentMarble % 23) != 0)
		{
			currentBoardIt = stepForward(begin(board), end(board), currentBoardIt, 2);
			currentBoardIt = board.insert(currentBoardIt, currentMarble);
		} else
		{
			scores[currentPlayer] += currentMarble;
			currentBoardIt = stepBack(begin(board), end(board), currentBoardIt, 7);
			scores[currentPlayer] += *currentBoardIt;
			currentBoardIt = board.erase(currentBoardIt);
		}
		currentPlayer++;
		if(currentPlayer == numPlayers)
			currentPlayer = 0;
	}

	auto maxIt = max_element(begin(scores), end(scores));
	int winner = static_cast<int>(distance(begin(scores), maxIt));
	auto winningScore = *maxIt;
	return make_pair(winner + 1, winningScore);
}

void solve_part1()
{
	auto [winner, winningScore] = playMarble(numPlayers, numMarbles);
	assert(winningScore == 428690);
	cout << dayName << " - part 1: " << winningScore << endl;
}

void solve_part2()
{
	auto [winner, winningScore] = playMarble(numPlayers, numMarbles * 100);
	assert(winningScore == 3628143500);
	cout << dayName << " - part 2: " << winningScore << endl;
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
