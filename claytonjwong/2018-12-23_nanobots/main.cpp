//
// Created by Clayton Wong on 2018-12-28.
//

#include "input.hpp"
#include <iostream>
#include <regex>
#include <sstream>
#include <tuple>
#include <vector>
#include <set>


using namespace std;
struct Nanobot
{
    int x{ 0 }, y{ 0 }, z{ 0 }, r{ 0 };

    bool inRange( const Nanobot& rhs ) const
    {
        return r >=
            abs( x - rhs.x ) +
            abs( y - rhs.y ) +
            abs( z - rhs.z );
    }
    bool operator<( const Nanobot& rhs ) const
    {
        return r < rhs.r;
    }
};
using Nanobots = vector< Nanobot >;


Nanobots getNanobots( const string& input, int x=0, int y=0, int z=0, int r=0,
    regex pattern=regex{ "^pos=<([-]?\\d+),([-]?\\d+),([-]?\\d+)>, r=(\\d+)$" }, Nanobots nanobots={} )
{
    istringstream stream{ input };
    for( string line; getline( stream, line ); )
    {
        smatch m; regex_match( line, m, pattern );
        if( m.size() == 5 )
        {
            stringstream parser; parser << m[1] << ' ' << m[2] << ' ' << m[3] << ' ' << m[4]; parser >> x >> y >> z >> r;
            nanobots.emplace_back( Nanobot{ x, y, z, r } );
        }
    }
    return nanobots;
}


int main()
{
    Nanobots nanobots = getNanobots( INPUT );
    auto strongest = *max_element( nanobots.cbegin(), nanobots.cend() );
    auto count = count_if( nanobots.cbegin(), nanobots.cend(),
        [&]( const auto& nanobot ){ return strongest.inRange( nanobot ); });
    cout << "answer part 1: " << count << endl; // 691

    return 0;
}
