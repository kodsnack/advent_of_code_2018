// Advent of Code 2018
// Peter Westerström (digimatic)

#include "config.h"

#include <common/common.h>

#include <algorithm>
#include <cassert>
#include <deque>
#include <functional>
#include <iostream>
#include <regex>
#include <stdexcept>
#include <string>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>

using namespace westerstrom;
using namespace std;
using namespace std::string_literals;
using Point = pair<int, int>;
using Carts = vector<string>;

namespace std
{
	template <> struct hash<Point>
	{
		size_t operator()(const Point& p) const noexcept
		{
			auto h1 = get<0>(p);
			auto h2 = get<1>(p);
			return h1 * 7919 + h2;
		}
	};
} // namespace std

using CartStates = unordered_map<pair<int, int>, int>;

constexpr bool isCart(char c) noexcept
{
	return (c == '<' || c == '>' || c == '^' || c == 'v');
}

template <typename Track, typename Carts>
void print(const Track& track, const Carts& carts, bool mapOnly = false)
{
	auto h = track.size();
	auto w = track[0].size();
	for(decltype(h) y = 0; y < h; ++y)
	{
		for(decltype(w) x = 0; x < w; ++x)
		{
			auto c = carts[y][x];
			auto m = track[y][x];
			if(!mapOnly && (isCart(c) || c == 'X'))
				cout << c;
			else
				cout << m;
		}
		cout << '\n';
	}
	cout << endl;
}

void moveCartForward(char c, int& x, int& y)
{
	switch(c)
	{
		case '>':
			x++;
			break;
		case '<':
			x--;
			break;
		case '^':
			y--;
			break;
		case 'v':
			y++;
			break;
		default:
			break;
	}
}

void turnCart(char& c, int ori)
{
	if(ori == -1)
	{
		switch(c)
		{
			case '<':
				c = 'v';
				break;
			case 'v':
				c = '>';
				break;
			case '>':
				c = '^';
				break;
			case '^':
				c = '<';
				break;
			default:
				break;
		}
	} else if(ori == 1)
	{
		switch(c)
		{
			case '<':
				c = '^';
				break;
			case 'v':
				c = '<';
				break;
			case '>':
				c = 'v';
				break;
			case '^':
				c = '>';
				break;
			default:
				break;
		}
	}
}

void maybeTurnCart(char& c, char m, int& n)
{
	if(m == '/')
	{
		if(c == '^')
		{
			c = '>';
		} else if(c == '<')
		{
			c = 'v';
		} else if(c == 'v')
		{
			c = '<';
		} else if(c == '>')
		{
			c = '^';
		}
	} else if(m == '\\')
	{
		if(c == '^')
		{
			c = '<';
		} else if(c == '<')
		{
			c = '^';
		} else if(c == 'v')
		{
			c = '>';
		} else if(c == '>')
		{
			c = 'v';
		}
	} else if(m == '+')
	{
		int turn = (n % 3) - 1; // -1 left, 0 straigt, 1 right
		turnCart(c, turn);
		n++;
	}
}

template <typename Track>
CartStates::value_type moveCart(const Track& track, char& c, int x, int y, int n)
{
	moveCartForward(c, x, y);
	maybeTurnCart(c, track[y][x], n);
	return make_pair(make_pair(x, y), n);
}
int collisionCount = 0;

template <typename Track, typename Carts>
pair<CartStates, Carts> update(const Track& track, Carts carts, const CartStates& cartStates)
{
	CartStates newCartStates;
	Carts newCarts = carts; // to get same size
	for(auto& l : newCarts)
	{
		for(auto& c : l)
		{
			if(!isCart(c))
				c = ' ';
		}
	}
	auto h = track.size();
	auto w = track[0].size();
	for(decltype(h) y = 0; y < h; ++y)
	{
		for(decltype(w) x = 0; x < w; ++x)
		{
			auto c = carts[y][x];
			auto m = track[y][x];
			if(isCart(c))
			{
				auto p0 = make_pair(x, y);
				auto n0 = cartStates.find(p0) != cartStates.end() ? cartStates.at(p0) : 0;
				auto [p, n] = moveCart(track, c, x, y, n0);
				if(newCarts[p.second][p.first] != ' ')
				{
					if(collisionCount++ == 0)
					{
						cout << dayName << " - part 1: " << p.first << "," << p.second << endl;
					}
					newCarts[p0.second][p0.first] = ' ';
					newCarts[p.second][p.first] = ' ';
					carts[p.second][p.first] = ' ';
					auto it = newCartStates.find(p);
					if(it != newCartStates.end())
						newCartStates.erase(it);
				} else
				{
					newCartStates[p] = n;
					newCarts[p0.second][p0.first] = ' ';
					newCarts[p.second][p.first] = c;
				}
			}
		}
	}
	return make_pair(newCartStates, newCarts);
}

template <typename Track> void removeCarts(Track& track)
{
	for(auto& trackRow : track)
	{
		for(auto& c : trackRow)
		{
			switch(c)
			{
				case '>':
					c = '-';
					break;
				case '<':
					c = '-';
					break;
				case '^':
					c = '|';
					break;
				case 'v':
					c = '|';
					break;
				default:
					break;
			}
		}
	}
}

void solve()
{
	auto track = readLines(string(inputFile));
	auto carts = track;
	removeCarts(track);

	CartStates cartStates;
	while(true)
	{
		auto r = update(track, carts, cartStates);
		cartStates = move(r.first);
		carts = move(r.second);
		assert(cartStates.size() != 0);
		if(cartStates.size() == 1)
		{
			auto it = cartStates.begin();
			cout << dayName << " - part 2: " << it->first.first << "," << it->first.second << endl;
			return;
		}
	}
	cout << dayName << " no solutions" << endl;
}

int main()
{
	solve();
	return 0;
}
