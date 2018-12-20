// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <deque>
#include <functional>
#include <iostream>
#include <queue>
#include <regex>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <numeric>
#include <stack>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

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
			auto h2 = std::hash<int>{}(p.y << 16);
			// Mainly for demonstration purposes, i.e. works but is overly simple
			// In the real world, use sth. like boost.hash_combine
			return h1 ^ h2;
		}
	};
} // namespace std

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

struct Room
{
	bool hasLeftDoor{false};
	bool hasUpperDoor{false};
	int shortestSteps{numeric_limits<int>::max()};
};
using Map = unordered_map<Point, Room>;

struct State
{
	Point p;
	int i{0};
	int steps{0};
};

pair<int,int> buildMap(string re)
{
	// Build jump-table for acceleration purpose
	vector<pair<int, int>> parents;
	parents.resize(re.size(), make_pair(-1,-1));
	{
		stack<pair<int, int>> c;
		int next = -1;
		int last = -1;
		for(int i=static_cast<int>(re.size())-1;i>=0;i--)
		{
			if(re[i] == ')')
			{
				c.emplace(next, next);
				last = i;
				next = i;
			} else if(re[i] == '|')
			{
				parents[i] = make_pair(next, last);
				next = i;
			} else if(re[i] == '(')
			{
				parents[i] = make_pair(next, last);
				next = c.top().first;
				last = c.top().second;
				c.pop();
			}
		}
		assert(next == -1);
		assert(last == -1);
	}

	Map m;
	m[Point{}].shortestSteps = 0;
	vector<State> q;
	q.push_back(State{});
	while(!q.empty())
	{
		auto& s = q.back();
		if(re[s.i] == '^')
		{
			s.i++;
		} else if(re[s.i] == 'W')
		{
			m[s.p].hasLeftDoor = true;
			s.p.x--;
			s.i++;
			s.steps++;
			if(s.steps < m[s.p].shortestSteps)
			{
				m[s.p].shortestSteps = s.steps;
			}
		} else if(re[s.i] == 'N')
		{
			m[s.p].hasUpperDoor = true;
			s.p.y--;
			s.i++;
			s.steps++;
			if(s.steps < m[s.p].shortestSteps)
			{
				m[s.p].shortestSteps = s.steps;
			}
		} else if(re[s.i] == 'E')
		{
			s.p.x++;
			m[s.p].hasLeftDoor = true;
			s.i++;
			s.steps++;
			if(s.steps < m[s.p].shortestSteps)
			{
				m[s.p].shortestSteps = s.steps;
			}
		} else if(re[s.i] == 'S')
		{
			s.p.y++;
			m[s.p].hasUpperDoor = true;
			s.i++;
			s.steps++;
			if(s.steps < m[s.p].shortestSteps)
			{
				m[s.p].shortestSteps = s.steps;
			}
		} else if(re[s.i] == '(')
		{
			auto s = q.back();
			q.pop_back();
			while(re[s.i]!=')')
			{
				s.i++;
				q.push_back(s);
				s.i--;

				s.i = parents[s.i].first;
			}
		} else if(re[s.i] == '|')
		{
			s.i = parents[s.i].second + 1;
		} else if(re[s.i] == ')')
		{
			s.i++;
		} else if(re[s.i] == '$')
		{
			q.pop_back();
		} else
		{
			assert(0);
		}

		sort(begin(q), end(q), [](auto&& s1, auto&& s2) {
			return std::tie(s1.p, s1.i, s1.steps) < std::tie(s2.p, s2.i, s2.steps);
		});
		q.erase(unique(begin(q), end(q), [](auto&& s1, auto&& s2) {
			return s1.p == s2.p && s1.i == s2.i;
		}), end(q));
	}

	auto it = max_element(begin(m), end(m), [](auto&& r1, auto&& r2) {
		return r1.second.shortestSteps < r2.second.shortestSteps;
	});

	int n = static_cast<int>(count_if(begin(m), end(m), [](auto&& r1) {
		return r1.second.shortestSteps >= 1000;
	}));

	return make_pair(it->second.shortestSteps, n);
}

void solve()
{
	// unit tests
	assert(3 == buildMap("^WNE$").first);
	assert(10 == buildMap("^ENWWW(NEEE|SSE(EE|N))$").first);
	assert(18 == buildMap("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$").first);
	assert(23 == buildMap("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$").first);
	assert(31 == buildMap("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$").first);

	// solutions
	auto re = readLines(string(inputFile));
	auto [r, n] = buildMap(re[0]);
	cout << dayName << " - part 1: " << r << endl;
	cout << dayName << " - part 2: " << n << endl;
	assert(r==3966);
	assert(n==8173);
}

int main()
{
	solve();
	return 0;
}
