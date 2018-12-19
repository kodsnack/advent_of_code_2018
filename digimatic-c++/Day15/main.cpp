// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <limits>
#include <numeric>
#include <optional>
#include <queue>
#include <set>
#include <string>
#include <tuple>
#include <unordered_set>
#include <utility>

using namespace westerstrom;
using namespace std;

const int verboseLevel = 0;

struct Point
{
	int x{0};
	int y{0};
};
struct Dir
{
	int x{0};
	int y{0};
};

Point make_point(int x, int y)
{
	return {x, y};
}

namespace std
{
	template <> struct hash<Point>
	{
		size_t operator()(const Point& p) const noexcept
		{
			auto h1 = std::hash<int>{}(p.x);
			auto h2 = std::hash<int>{}(p.y * 7919);
			// Mainly for demonstration purposes, i.e. works but is overly simple
			// In the real world, use sth. like boost.hash_combine
			return h1 ^ h2;
		}
	};
} // namespace std

Point operator+(Point p1, Dir p2)
{
	return make_point(p1.x + p2.x, p1.y + p2.y);
}
Point operator-(Point p1, Dir p2)
{
	return make_point(p1.x - p2.x, p1.y - p2.y);
}

using Combat = pair<Point, int>;
using Combats = vector<Combat>;
using Map = vector<string>;

bool operator<(Point p, Point q) noexcept
{
	return tie(p.y, p.x) < tie(q.y, q.x);
}
bool operator==(Point p, Point q) noexcept
{
	return tie(p.x, p.y) == tie(q.x, q.y);
}
bool operator!=(Point p, Point q) noexcept
{
	return tie(p.x, p.y) != tie(q.x, q.y);
}
bool operator==(Dir p, Dir q) noexcept
{
	return tie(p.x, p.y) == tie(q.x, q.y);
}
bool operator!=(Dir p, Dir q) noexcept
{
	return tie(p.x, p.y) != tie(q.x, q.y);
}

char getAt(const Map& map, Point p)
{
	return map[p.y][p.x];
}
void setAt(Map& map, Point p, char c)
{
	map[p.y][p.x] = c;
}

bool isEmpty(const Map& map, Point p) noexcept
{
	return getAt(map, p) == '.';
}

bool isEnemy(char c1, char c2) noexcept
{
	return (c1 == 'G' && c2 == 'E') || (c1 == 'E' && c2 == 'G');
}

bool isEnemy(const Map& m, Point p, char c)
{
	return isEnemy(c, getAt(m, p));
}

bool isCombat(char c) noexcept
{
	return (c == 'G' || c == 'E');
}

bool isWall(char c) noexcept
{
	return (c == '#');
}

const vector<Dir> dirs = {{0, -1}, {-1, 0}, {1, 0}, {0, 1}};

Point findClosestEnemy(Map m, Point p)
{
	char c = getAt(m, p);
	queue<Point> q;
	int steps = -1;
	set<Point> found;
	setAt(m, p, '*'); // visited
	q.push(p);
	while(found.empty() && !q.empty())
	{
		queue<Point> q2;
		while(!q.empty())
		{
			auto pp = q.front();
			q.pop();
			for(auto d : dirs)
			{
				auto p1 = pp + d;
				if(isEnemy(m, p1, c))
				{
					found.insert(pp);
				} else if(isEmpty(m, p1))
				{
					setAt(m, p1, '*');
					q2.push(p1);
				}
			}
		}
		q = move(q2);
		steps++;
	}

	if(verboseLevel >= 2)
		cout << p.x << "," << p.y << ": " << steps << endl;
	if(found.empty())
		return make_point(0, 0);
	else
	{
		if(verboseLevel >= 2)
		{
			for(auto pp : found)
				cout << "  " << pp.x << "," << pp.y << endl;
		}
		return *(found.begin());
	}
}

template <typename F> void forAllMapPoints(const Map& m, F&& f)
{
	int h = static_cast<int>(m.size());
	int w = static_cast<int>(m[0].size());
	for(int y = 0; y < h; ++y)
	{
		for(int x = 0; x < w; ++x)
		{
			auto p = make_point(x, y);
			char c = getAt(m, p);
			invoke(f, p, c);
		}
	}
}

Dir findCombatStep(const Map& m, Point p)
{
	auto selEnemyPos = findClosestEnemy(m, p);
	if(selEnemyPos == make_point(0, 0))
		return {};

	unordered_set<Point> visited;
	queue<tuple<Point, int, Dir>> q;

	char c = getAt(m, p);
	visited.insert(p);
	for(auto d : dirs)
	{
		q.push(make_tuple(p + d, 1, d));
		visited.insert(p + d);
	}

	while(!q.empty())
	{
		auto [pp, steps, stepDir] = q.front();
		q.pop();

		if(pp == selEnemyPos)
		{
			return stepDir;
		} else if(isEmpty(m, pp))
		{
			for(auto d : dirs)
			{
				auto v = pp + d;
				if(visited.find(v) == visited.end())
				{
					visited.insert(v);
					q.push(make_tuple(v, steps + 1, stepDir));
				}
			}
		}
	}
	return {};
}

Combat& getCombatAt(Combats& combats, Point p)
{
	auto it = find_if(begin(combats), end(combats), [p](auto&& c) { return c.first == p; });
	assert(it != end(combats));
	return *it;
}

bool tryCombat(Map& m, Combats& combats, Point p, Map& am, int elveAttackPower)
{
	char c = getAt(m, p);
	assert(isCombat(c));
	Dir bestDir = {0, 0};
	int lowestHP{100000};
	for(int dirIndex = 0; dirIndex < 4; ++dirIndex)
	{
		auto p1 = p + dirs[dirIndex];
		auto c1 = getAt(m, p1);
		if(isEnemy(c, c1))
		{
			auto& enemy = getCombatAt(combats, p1);
			if(enemy.second < lowestHP)
			{
				lowestHP = enemy.second;
				bestDir = dirs[dirIndex];
			}
		}
	}
	if(bestDir != Dir{0, 0})
	{
		setAt(am, p, bestDir.x > 0 ? '>' : (bestDir.x < 0 ? '<' : (bestDir.y < 0 ? '^' : 'v')));
		auto p1 = p + bestDir;
		auto& enemy = getCombatAt(combats, p1);
		if(c == 'E')
		{
			enemy.second -= elveAttackPower;
		} else
		{
			enemy.second -= 3;
		}
		if(enemy.second <= 0)
		{
			setAt(m, p1, '.');
			enemy.first = make_point(-1, -1);
		}
		return true;
	}
	return false;
}

bool isFinished(const Map& m, const Combats& combats)
{
	bool hasElves = find_if(begin(combats), end(combats), [&m](auto&& c) {
		                return c.second > 0 && getAt(m, c.first) == 'E';
	                }) != end(combats);
	bool hasGuards = find_if(begin(combats), end(combats), [&m](auto&& c) {
		                 return c.second > 0 && getAt(m, c.first) == 'G';
	                 }) != end(combats);
	return (hasElves && !hasGuards) || (!hasElves && hasGuards);
}

bool playRound(Map& m, Combats& combats, Map& am, int elveAttackPower)
{
	am = m;
	for(auto& combat : combats)
	{
		if(isFinished(m, combats))
		{
			combats.erase(
			    remove_if(begin(combats), end(combats), [](auto&& c) { return c.second <= 0; }),
			    end(combats));
			return false;
		}

		if(combat.second <= 0)
			continue;

		auto p = combat.first;
		char c = getAt(m, p);
		if(isCombat(c))
		{
			assert(combat.second > 0);
			if(!tryCombat(m, combats, p, am, elveAttackPower))
			{
				auto dir = findCombatStep(m, p);
				if(dir != Dir{0, 0})
				{
					// move
					auto newPos = p + dir;
					setAt(m, newPos, c);
					setAt(m, p, '.');
					combat.first = newPos;
					tryCombat(m, combats, newPos, am, elveAttackPower);
				}
			}
		}
	}
	combats.erase(remove_if(begin(combats), end(combats), [](auto&& c) { return c.second <= 0; }),
	              end(combats));
	return true;
}

void sort(Combats& c)
{
	sort(c.begin(), c.end(), [](auto&& c1, auto&& c2) { return c1.first < c2.first; });
}

void print(int r, const Map& m0, const Map& am, const Map& m, const Combats& combats)
{
	cout << r << "\n";
	int h = static_cast<int>(m.size());
	int w = static_cast<int>(m[0].size());
	for(int y = 0; y < h; ++y)
	{
		cout << m0[y] << " " << am[y] << " " << m[y] << "  ";
		for(auto& c : combats)
		{
			if(c.first.y == y)
			{
				cout << getAt(m, c.first) << "(" << c.second << ") ";
			}
		}
		cout << "\n";
	}
	cout << flush;
}

int hitPointSum(const Map& m, const Combats& combats)
{
	return accumulate(begin(combats), end(combats), 0,
	                  [&m](int acc, auto&& c) { return c.second + acc; });
}

Combats createCombatsList(const Map& m)
{
	Combats combats;
	int h = static_cast<int>(m.size());
	int w = static_cast<int>(m[0].size());
	for(int y = 0; y < h; ++y)
	{
		for(int x = 0; x < w; ++x)
		{
			auto p = make_point(x, y);
			char c = getAt(m, p);
			if(isCombat(c))
			{
				combats.push_back(make_pair(p, 200));
			}
		}
	}
	sort(combats);
	return combats;
}

tuple<int, int, int> doGame(Map& m)
{
	auto combats = createCombatsList(m);
	int rounds = 0;
	auto am = m;
	if(verboseLevel >= 2)
		print(rounds, m, am, m, combats);
	while(true)
	{
		if(isFinished(m, combats))
			break;
		if(verboseLevel >= 2)
			cout << "\nStarting round " << (rounds + 1) << "\n";
		auto m0 = m;
		bool finishedRound = playRound(m, combats, am, 3);
		sort(combats);
		if(!finishedRound)
			break;
		rounds++;

		if(verboseLevel >= 1)
			print(rounds, m0, am, m, combats);
		// getchar();
	}
	if(verboseLevel >= 1)
		print(rounds, m, am, m, combats);
	int sum = hitPointSum(m, combats);
	int r = sum * rounds;
	return make_tuple(rounds, sum, r);
}

int countElves(const Map& m)
{
	int n = 0;
	forAllMapPoints(m, [&n](auto&& p, auto&& c) {
		if(c == 'E')
			n++;
	});
	return n;
}

optional<tuple<int, int, int>> doElveGame(Map& m, int elveAttackPower)
{
	auto combats = createCombatsList(m);
	int rounds = 0;
	auto am = m;
	int elvesFromStart = countElves(m);
	while(true)
	{
		if(isFinished(m, combats))
			break;
		auto m0 = m;
		bool finishedRound = playRound(m, combats, am, elveAttackPower);
		sort(combats);
		if(countElves(m) < elvesFromStart)
			return nullopt;
		if(!finishedRound)
			break;
		rounds++;
	}
	int sum = hitPointSum(m, combats);
	int r = sum * rounds;
	return make_tuple(rounds, sum, r);
}

void unitTest0()
{
	vector<string> testMap0 = {"#######", "#.G...#", "#...EG#", "#.#.#G#",
	                           "#..G#E#", "#.....#", "#######"};
	auto [rounds, sum, r] = doGame(testMap0);
	cout << "rounds=" << rounds << ", sum=" << sum << " r=" << r << endl;
	assert(rounds == 47 && r == 47 * 590 && "Test0");
}
void unitTest1()
{
	vector<string> testMap1 = {"#######", "#G..#E#", "#E#E.E#", "#G.##.#",
	                           "#...#E#", "#...E.#", "#######"};
	auto [rounds, sum, r] = doGame(testMap1);
	cout << "rounds=" << rounds << ", sum=" << sum << " r=" << r << endl;
	assert(rounds == 37 && r == 36334 && "Test1");
}
void unitTest2()
{
	vector<string> testMap2 = {"#######", "#E..EG#", "#.#G.E#", "#E.##E#",
	                           "#G..#.#", "#..E#.#", "#######"};
	auto [rounds, sum, r] = doGame(testMap2);
	cout << "rounds=" << rounds << ", sum=" << sum << " r=" << r << endl;
	assert(rounds == 46 && r == 39514 && "Test2");
}
void unitTest3()
{
	vector<string> testMap2 = {"#######", "#E.G#.#", "#.#G..#", "#G.#.G#",
	                           "#G..#.#", "#...E.#", "#######"};
	auto [rounds, sum, r] = doGame(testMap2);
	cout << "rounds=" << rounds << ", sum=" << sum << " r=" << r << endl;
	assert(rounds == 35 && r == 27755 && "Test3");
}
void unitTest4()
{
	vector<string> testMap2 = {"#######", "#.E...#", "#.#..G#", "#.###.#",
	                           "#E#G#G#", "#...#G#", "#######"};
	auto [rounds, sum, r] = doGame(testMap2);
	cout << "rounds=" << rounds << ", sum=" << sum << " r=" << r << endl;
	assert(rounds == 54 && r == 28944 && "Test4");
}
void unitTest5()
{
	vector<string> testMap2 = {"#########", "#G......#", "#.E.#...#", "#..##..G#", "#...##..#",
	                           "#...#...#", "#.G...G.#", "#.....G.#", "#########"};
	auto [rounds, sum, r] = doGame(testMap2);
	cout << "rounds=" << rounds << ", sum=" << sum << " r=" << r << endl;
	assert(rounds == 20 && r == 18740 && "Test5");
}

void unitTest6()
{
	vector<string> testMap = {"#######", "#E..G.#", "#...#.#", "#.G.#G#", "#######"};

	auto p = findClosestEnemy(testMap, make_point(1, 1));
	assert(p.x == 3 && p.y == 1 && "Test6");
}
void unitTests()
{
	assert(make_point(2, 1) < make_point(2, 2));
	assert(make_point(3, 1) < make_point(2, 2));
	assert(make_point(1, 2) < make_point(2, 2));

	assert(!(make_point(2, 2) < make_point(2, 1)));
	assert(!(make_point(2, 2) < make_point(3, 1)));
	assert(!(make_point(2, 2) < make_point(1, 2)));

	unitTest6();
	unitTest0();
	unitTest1();
	unitTest2();
	unitTest3();
	unitTest4();
	unitTest5();
}

void solve_part1()
{
	// Part 1
	auto m = readLines(string(inputFile));
	auto [rounds, sum, r] = doGame(m);
	cout << dayName << " - part 1: #rounds=" << rounds << ", sum=" << sum << " OUTCOME=" << r
	     << endl;
	assert(r == 237996);
}

void solve_part2()
{
	// Part 2
	auto mIn = readLines(string(inputFile));
	int elveAttackPower = 3;
	while(true)
	{
		elveAttackPower++;
		auto m = mIn;
		auto gameRes = doElveGame(m, elveAttackPower);
		if(gameRes)
		{
			auto [rounds, sum, r] = *gameRes;
			cout << dayName << " - part 2: #rounds=" << rounds << ", sum=" << sum
			     << " OUTCOME=" << r << ", elveAttackPower=" << elveAttackPower << endl;
			assert(r == 69700);
			return;
		}
	}
}

int main()
{
	// unitTests();
	solve_part1();
	solve_part2();
	return 0;
}
