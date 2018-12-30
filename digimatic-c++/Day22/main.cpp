// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <deque>
#include <functional>
#include <iostream>
#include <optional>
#include <queue>
#include <regex>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

struct Point
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

// intput
const auto depth = 9465;
const auto target = Point{13, 704};
// const auto depth = 510;
// const auto target = Point{10, 10};

struct Region
{
	int index;
	int type;
};

int getType(int errosionLevel)
{
	return errosionLevel % 3;
}

int computeErrosionLevel(int index)
{
	return (index + depth) % 20183;
}

enum class Tool : int
{
	Neither,
	Torch,
	ClimbingGear
};

struct State
{
	Point p;
	Tool tool;
};
bool operator==(const State& s1, const State& s2)
{
	return s1.p == s2.p && s1.tool == s2.tool;
}

namespace std
{
	template <> struct hash<State>
	{
		size_t operator()(const State& s) const noexcept
		{
			auto h1 = std::hash<Point>{}(s.p);
			auto h2 = std::hash<int>{}(static_cast<int>(s.tool) << 18);
			// Mainly for demonstration purposes, i.e. works but is overly simple
			// In the real world, use sth. like boost.hash_combine
			return h1 ^ h2;
		}
	};
} // namespace std

void solve()
{
	unordered_map<Point, Region> regions;
	regions[Point{0, 0}] = Region{0};

	// max size, tune this for input
	int w = target.x * 100;
	int h = target.y * 5;

	int totalRisk = 0;
	for(int x = 0; x <= w; ++x)
	{
		for(int y = 0; y <= h; ++y)
		{
			auto p = Point{x, y};
			int index;
			if(p == target)
			{
				index = 0;
			} else if(x == 0)
			{
				index = y * 48271;
			} else if(y == 0)
			{
				index = x * 16807;
			} else
			{
				assert(regions.find(Point{x - 1, y}) != regions.end());
				assert(regions.find(Point{x, y - 1}) != regions.end());
				index = computeErrosionLevel(regions[Point{x - 1, y}].index) *
				        computeErrosionLevel(regions[Point{x, y - 1}].index);
			}
			int type = getType(computeErrosionLevel(index));
			regions[p] = Region{index, type};

			if(x <= target.x && y <= target.y)
				totalRisk += type;
		}
	}
	cout << dayName << " - part 1: " << totalRisk << endl;
	assert(totalRisk == 9940);

	// part 2
	queue<State> q;
	unordered_map<State, int> visited;
	q.push(State{Point{0, 0}, Tool::Torch});
	visited[State{Point{0, 0}, Tool::Torch}] = 0;

	auto targetState = State{target, Tool::Torch};

	auto isAllowedTool = [&](Point p, Tool tool) {
		auto type = regions[p].type;
		if(type == 0) // rocky
			return (tool == Tool::ClimbingGear || tool == Tool::Torch);
		else if(type == 1) // wet
			return (tool == Tool::ClimbingGear || tool == Tool::Neither);
		else // 2: // narrow
			return (tool == Tool::Torch || tool == Tool::Neither);
	};

	auto isPossible = [&](const State& s) {
		if(s.p.x < 0 || s.p.y < 0)
			return false;
		if(s.p.x > (w + 1) || s.p.y > (h + 1))
		{
			cout << "outside at " << s.p.x << "," << s.p.y << ", increase max size" << endl;
			return false;
		}
		return isAllowedTool(s.p, s.tool);
	};

	auto addPossibleSteps = [&](State s, int mins) {
		for(int dy = -1; dy <= 1; dy++)
		{
			for(int dx = -1; dx <= 1; dx++)
			{
				for(int tool = 0; tool <= 2; tool++)
				{
					Tool nt = static_cast<Tool>(tool);
					if(abs(dx) != abs(dy) || (dx == 0 && dy == 0 && nt != s.tool))
					{
						auto next = s;
						next.p.x += dx;
						next.p.y += dy;
						next.tool = nt;
						if(isAllowedTool(s.p, next.tool) && isPossible(next))
						{
							int minsNext = mins;
							minsNext += dx != 0 || dy != 0 ? 1 : 0;
							minsNext += nt != s.tool ? 7 : 0;

							auto targetIt = visited.find(targetState);
							int bestSoFar = (targetIt == visited.end()) ? numeric_limits<int>::max()
							                                            : (targetIt->second);
							if(minsNext < bestSoFar)
							{
								if(visited.find(next) == visited.end() || minsNext < visited[next])
								{
									visited[next] = minsNext;
									q.push(next);
								}
							}
						}
					}
				}
			}
		}
	};
	while(!q.empty())
	{
		auto s = q.front();
		q.pop();
/*		if(s == targetState)
		{
			cout << visited[targetState] << "  #q" << q.size() << endl;
		}*/

		int mins = visited[s];
		addPossibleSteps(s, mins);
	}

	cout << dayName << " - part 2: ";
	auto targetIt = visited.find(targetState);
	if(targetIt != end(visited))
	{
		cout << visited[targetState] << endl;
		assert(visited[targetState] == 944);
	} else
	{
		cout << "No solution found" << endl;
		assert(false);
	}
}

int main()
{
	solve();
	return 0;
}
