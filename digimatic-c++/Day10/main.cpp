// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <algorithm>
#include <common/common.h>
#include <functional>
#include <iostream>
#include <string>
#include <utility>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;

using Point = pair<int, int>;
using Vel = pair<int, int>;
using PointVel = pair<Point, Vel>;
using Points = vector<PointVel>;
using Rect = pair<Point, Point>;

bool operator<(Point p, Point q) noexcept
{
	if(get<1>(p) < get<1>(q))
		return true;
	if(get<1>(p) == get<1>(q))
	{
		return (get<0>(p) < get<0>(q));
	}
	return false;
}

PointVel parseLine(const string& line)
{
	auto remaining = line;
	remaining = remaining.substr(10);
	size_t taken = 0;
	auto x = std::stoi(remaining, &taken);
	remaining = remaining.substr(taken + 1);
	auto y = std::stoi(remaining, &taken);
	remaining = remaining.substr(taken + 12);
	auto vx = std::stoi(remaining, &taken);
	remaining = remaining.substr(taken + 1);
	auto vy = std::stoi(remaining, &taken);

	return make_tuple(make_tuple(x, y), make_tuple(vx, vy));
}

Points parseLines(const vector<string>& lines)
{
	Points pointvels;
	for(auto& line : lines)
	{
		pointvels.push_back(parseLine(line));
	}
	return pointvels;
}

void update(Points& points)
{
	for(auto& [p, v] : points)
	{
		p.first += v.first;
		p.second += v.second;
	}
}
void updateRev(Points& points)
{
	for(auto& [p, v] : points)
	{
		p.first -= v.first;
		p.second -= v.second;
	}
}

Rect canvasExtents(const Points& points)
{
	Point minP = points[0].first;
	Point maxP = points[0].first;
	for(auto [p, v] : points)
	{
		minP.first = min(minP.first, p.first);
		maxP.first = max(maxP.first, p.first);

		minP.second = min(minP.second, p.second);
		maxP.second = max(maxP.second, p.second);
	}
	return make_pair(minP, maxP);
}

int rectSize(Rect r)
{
	return abs(r.second.first - r.first.first) + abs(r.second.second - r.first.second);
}

void paintPoints(Points points)
{
	auto r = canvasExtents(points);
	int x0 = r.first.first;
	int y0 = r.first.second;
	int w = r.second.first - r.first.first + 1;
	int h = r.second.second - r.first.second + 1;
	vector<vector<char>> c;
	{
		vector<char> lineTemplate(w, '.');
		c.resize(h, lineTemplate);
	}
	for(auto [p, v] : points)
	{
		c[p.second - y0][p.first - x0] = '#';
	}
	for(int y = 0; y < h; y++)
	{
		for(int x = 0; x < w; x++)
		{
			cout << c[y][x];
		}
		cout << '\n';
	}
	cout << flush;
}

void solve()
{
	auto points = parseLines(readLines(string(inputFile)));
	auto ext = canvasExtents(points);
	auto minSize = rectSize(ext);
	int i = 0;
	int n = 0;
	while(true)
	{
		update(points);
		auto s = rectSize(canvasExtents(points));
		if(s > minSize)
		{
			break;
		}
		minSize = s;
		++i;
	}
	updateRev(points);

	cout << dayName << " - part 1: " << endl;
	paintPoints(points);

	cout << dayName << " - part 2: " << i << endl;
}

int main()
{
	solve();
	return 0;
}
