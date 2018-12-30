// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>

using namespace westerstrom;
using namespace std;

using Nanobot = tuple<int64_t, int64_t, int64_t, int64_t>;
using Nanobots = vector<Nanobot>;
using Point = tuple<int64_t, int64_t, int64_t>;

inline std::vector<long long> splitNumbers64(const std::string& line)
{
	using namespace std;
	vector<long long> ints;
	regex re("[^0-9\\-]+");
	transform(sregex_token_iterator(line.begin(), line.end(), re, -1), sregex_token_iterator(),
	          back_inserter(ints), [](const auto& s) -> long long {
		          try
		          {
			          return stoll(s);
		          } catch(const invalid_argument&)
		          {
			          return 0;
		          }
	          });
	return ints;
}

Nanobot parseLine(const string& line)
{
	auto nums = splitNumbers64(line);
	return make_tuple(nums[1], nums[2], nums[3], nums[4]);
}

Nanobots parseLines(const vector<string>& lines)
{
	Nanobots parsedLines;
	for(auto& line : lines)
	{
		parsedLines.push_back(parseLine(line));
	}
	return parsedLines;
}

Point operator+(Point p1, Point p2)
{
	return make_tuple(get<0>(p1) + get<0>(p2), get<1>(p1) + get<1>(p2), get<2>(p1) + get<2>(p2));
}

Point operator*(Point p1, int64_t n)
{
	return make_tuple(get<0>(p1) * n, get<1>(p1) * n, get<2>(p1) * n);
}
Point operator/(Point p1, int64_t n)
{
	return make_tuple(get<0>(p1) / n, get<1>(p1) / n, get<2>(p1) / n);
}

ostream& operator<<(ostream& os, Point p)
{
	os << "(" << get<0>(p) << "," << get<1>(p) << "," << get<2>(p) << ")";
	return os;
}

Point getPos(const Nanobot& b)
{
	return make_tuple(get<0>(b), get<1>(b), get<2>(b));
}

auto getRange(const Nanobot& b)
{
	return get<3>(b);
}

auto distance(Nanobot b, Nanobot b0)
{
	auto l =
	    abs(get<0>(b) - get<0>(b0)) + abs(get<1>(b) - get<1>(b0)) + abs(get<2>(b) - get<2>(b0));
	return l;
}

auto distance(Nanobot b, Point p0)
{
	auto l =
	    abs(get<0>(b) - get<0>(p0)) + abs(get<1>(b) - get<1>(p0)) + abs(get<2>(b) - get<2>(p0));
	return l;
}

auto distance(Point b, Point p0)
{
	auto l =
	    abs(get<0>(b) - get<0>(p0)) + abs(get<1>(b) - get<1>(p0)) + abs(get<2>(b) - get<2>(p0));
	return l;
}

auto botsInRangeOf(const Nanobots& bots, Nanobot b0)
{
	auto n = count_if(begin(bots), end(bots), [b0](auto&& b) {
		auto l = distance(b, b0);
		return l <= getRange(b0);
	});
	return n;
}

auto botsInRangeOf(const Nanobots& bots, Point p0)
{
	auto n = count_if(begin(bots), end(bots), [p0](auto&& b) {
		auto l = distance(b, p0);
		return l <= get<3>(b);
	});
	return n;
}

void solve_part1()
{
	auto nanobots = parseLines(readLines(string(inputFile)));
	auto maxIt = max_element(begin(nanobots), end(nanobots),
	                         [](auto&& b1, auto&& b2) { return getRange(b1) < getRange(b2); });
	auto b0 = (*maxIt);
	auto count = botsInRangeOf(nanobots, b0);
	cout << dayName << " - part 1: " << count << endl;
	assert(count == 399);
}

void solve_part2()
{
	auto nanobots = parseLines(readLines(string(inputFile)));

	vector<Point> bestPoints;
	bestPoints.push_back(Point{0, 0, 0});
	int64_t d = 5;
	int64_t dist = 100000000;
	int64_t bestCount = 0;
	int64_t bestZeroDist = 0;
	while(dist >= 1)
	{
		for(decltype(d) x = 0; x < d; x += 1)
		{
			for(decltype(d) y = 0; y < d; y += 1)
			{
				for(decltype(d) z = 0; z < d; z += 1)
				{
					Point o = make_tuple(x, y, z);
					o = o * (dist / d) + Point{-1, -1, -1} * (dist / 2);

					for(int i = 0; i < bestPoints.size(); i++)
					{
						auto pp = bestPoints[i] + o;
						auto nn = botsInRangeOf(nanobots, pp);
						auto zeroDist = distance(pp, Point{0, 0, 0});
						if(nn > bestCount)
						{
							bestPoints.clear();
							bestPoints.push_back(pp);
							bestCount = nn;
							bestZeroDist = zeroDist;
							cout << "New best #=" << nn << ", at " << pp << endl;
						} else if(nn == bestCount)
						{
							auto d1 = distance(bestPoints[0], Point{0, 0, 0});
							auto d2 = distance(bestPoints[bestPoints.size() - 1], Point{0, 0, 0});
							if(zeroDist < d1 && zeroDist < d2)
							{
								bestPoints.push_back(pp);
								cout << "New better #=" << nn << ", at " << pp << endl;
								bestZeroDist = zeroDist;
							}
						}
					}
				}
			}
		}
		cout << "Dist: " << dist << ", best count = " << bestCount << ", zd=" << bestZeroDist
		     << endl;
		dist /= 2;
	}

	cout << "Compute result:" << endl;
	for(auto p : bestPoints)
	{
		cout << get<0>(p) << "," << get<1>(p) << "," << get<2>(p) << endl;
		auto md0 = distance(p, Point{0, 0, 0});
		cout << " " << md0 << endl;
		cout << " bir=" << botsInRangeOf(nanobots, p) << endl;
	}

	cout << dayName << " - part 2: " << bestZeroDist << endl;
	assert(bestZeroDist == 81396996);
	// 81396996 (15394105,24778490,41224401)
}

int main()
{
	solve_part1();
	solve_part2();
	return 0;
}
