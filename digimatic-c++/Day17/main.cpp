// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <numeric>
#include <optional>
#include <queue>
#include <string>
#include <utility>

using namespace westerstrom;
using namespace std;

using Range = pair<int, int>;
using Clay = pair<Range, Range>;
struct Point
{
	int x, y;
};
struct Map
{
	int x0;
	int w, h;
	vector<string> area;
	int minY;
};

void drawAt(Map& m, Point p, char c)
{
	m.area[p.y][p.x - m.x0] = c;
}
optional<char> getAt(const Map& m, Point p)
{
	if(p.x < m.x0 || p.x >= (m.x0 + m.w))
		return nullopt;
	if(p.y < 0 || p.y >= m.h)
		return nullopt;
	return m.area.at(p.y)[p.x - m.x0];
}

vector<Clay> parseLines(const vector<string>& lines)
{
	vector<Clay> clays;
	for(auto& line : lines)
	{
		auto v = splitNumbers(line);
		if(line[0] == 'x')
		{
			clays.push_back(make_pair(make_pair(v[1], v[1]), make_pair(v[2], v[3])));
		} else
		{
			clays.push_back(make_pair(make_pair(v[2], v[3]), make_pair(v[1], v[1])));
		}
	}
	return clays;
}

Map genMap(const vector<Clay>& clays)
{
	auto [hr, vr] =
	    accumulate(begin(clays), end(clays), make_pair(make_pair(500, 500), clays[0].second),
	               [](auto&& a, auto&& b) {
		               return make_pair(make_pair(min(a.first.first, b.first.first),
		                                          max(a.first.second, b.first.second)),
		                                make_pair(min(a.second.first, b.second.first),
		                                          max(a.second.second, b.second.second)));
	               });
	hr.first -= 2;
	hr.second += 2;
	Map m;
	m.x0 = hr.first;
	m.minY = vr.first;
	m.w = hr.second + 1 - hr.first;
	m.h = vr.second + 1;
	string l;
	for(int x = hr.first; x <= hr.second; x++)
		l += '.';
	m.area.resize(m.h, l);
	drawAt(m, Point{500, 0}, '+');

	for(auto [xr, yr] : clays)
	{
		for(int y = yr.first; y <= yr.second; y++)
		{
			for(int x = xr.first; x <= xr.second; x++)
			{
				drawAt(m, Point{x, y}, '#');
			}
		}
	}
	return m;
}

void printMap(const Map& m)
{
	for(auto& l : m.area)
	{
		for(auto& c : l)
		{
			cout << c;
		}
		cout << "\n";
	}
	cout << flush;
}
void printSubMap(const Map& m, Point p, int s)
{
	for(int y = p.y - s / 8; y <= p.y + s / 4; ++y)
	{
		for(int x = p.x - s; x <= p.x + s; ++x)
		{
			if(auto c = getAt(m, Point{x, y}))
			{
				cout << *c;
			} else
			{
				cout << '%';
			}
		}
		cout << "\n";
	}
	cout << flush;
}

void solve()
{
	const bool verbose = false;
	const bool interactive = true;

	auto clays = parseLines(readLines(string(inputFile)));
	auto m = genMap(clays);
	queue<Point> flowEdges;
	flowEdges.push(Point{500, m.minY});
	int waterArea = 0;
	while(!flowEdges.empty())
	{
		auto p = flowEdges.front();
		flowEdges.pop();
		auto c = getAt(m, p);
		if(!c)
			continue;

		bool dropCurrent = false;
		while(true)
		{
			c = getAt(m, Point{p.x, p.y});
			if(!c || *c != '.')
				break;
			drawAt(m, p, '|');
			waterArea++;
			auto cd = getAt(m, Point{p.x, p.y + 1});
			if(cd)
			{
				if(cd == '|')
				{
					dropCurrent = true;
					break;
				} else if(cd == '#' || cd == '~')
				{
					break;
				}
			}
			p.y++;
		}
		if(dropCurrent)
			continue;
		c = getAt(m, p);
		if(!c)
			continue;

		if(c == '|')
		{
			// find limits
			bool open[2] = {true, true};
			bool openSand[2]{false, false};
			int limits[2] = {m.x0, m.x0 + m.w - 1};
			for(int dir = -1, i = 0; dir <= 1; dir += 2, i++)
			{
				for(int x = p.x; x < (m.x0 + m.w) && x >= m.x0; x += dir)
				{
					auto cc = getAt(m, Point{x, p.y});
					if(*cc != '.' && *cc != '|') // ie # (~ not possible here)
					{
						open[i] = false;
						limits[i] = x - dir;
						break;
					}
					auto ccd = getAt(m, Point{x, p.y + 1});
					if(*cc == '|' && ccd && *ccd == '|')
					{
						open[i] = true;
						openSand[i] = false;
						limits[i] = x - dir;
						break;
					}
					if(ccd && (*ccd == '.' || *ccd == '|')) // below is sand ?
					{                                       // start to fall again
						open[i] = true;
						openSand[i] = *ccd == '.';
						limits[i] = x - dir;
						break;
					}
				}
			}
			char fillChar = open[0] || open[1] ? '|' : '~';
			for(int x = limits[0]; x <= limits[1]; ++x)
			{
				if(*getAt(m, Point{x, p.y}) == '.')
					waterArea++;
				drawAt(m, Point{x, p.y}, fillChar);
			}
			for(int i = 0; i < 2; i++)
			{
				if(open[i] && openSand[i])
					flowEdges.push(Point{limits[i] + (i == 0 ? -1 : 1), p.y});
			}
			if(!open[0] && !open[1])
				flowEdges.push(Point{p.x, p.y - 1});
		} else if(c == '~')
		{
			auto cu = getAt(m, Point{p.x, p.y - 1});
			if(cu && *cu == '|')
			{
				flowEdges.push(Point{p.x, p.y - 1});
			}
		}
		if(verbose)
		{
			if(!flowEdges.empty())
			{
				auto p = flowEdges.front();
				auto c = getAt(m, p);
				drawAt(m, p, '?');
				printSubMap(m, p, 80);
				drawAt(m, p, *c);
			} else
			{
				printSubMap(m, p, 80);
			}
			cout << " #queue: " << flowEdges.size() << endl;
			//		cout << "Area: " << waterArea << endl << endl;
			if(interactive)
				getchar();
		}
	}
	if(verbose)
		printMap(m);
	cout << dayName << " - part 1: " << waterArea << endl;
	assert(waterArea == 50838);

	int stillWaterArea = 0;
	for(auto l : m.area)
	{
		stillWaterArea += count_if(begin(l), end(l), [](char c) { return c == '~'; });
	}
	cout << dayName << " - part 2: " << stillWaterArea << endl;
	assert(stillWaterArea == 43039);
}

int main()
{
	solve();
	return 0;
}
