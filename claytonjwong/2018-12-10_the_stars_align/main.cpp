//
// Created by Clayton Wong on 2018-12-16.
//

#include "input.hpp"
#include <iostream>
#include <vector>
#include <regex>
#include <sstream>
#include <unordered_set>


struct Position{ int row{ 0 }, col{ 0 }; };
struct Velocity{ int row{ 0 }, col{ 0 }; };
struct Light{ Position pos; Velocity speed; };


using namespace std;
using Lights = vector< Light >;
using Aligned = vector< int >; // unique alignments of row/col per tick
using Unique = unordered_set< int >;
bool operator==( const Position& lhs, const Position& rhs ){ return lhs.row == rhs.row && lhs.col == rhs.col; }
struct Cmp{ size_t operator()( const Position& pos ) const { return pos.row * 10000 + pos.col; } };
using Message = unordered_set< Position, Cmp >;


struct Answer
{
    Lights lights;
    int tick{ 0 }, minRow{ 0 }, maxRow{ 0 }, minCol{ 0 }, maxCol{ 0 };
};


class Solution
{
public:
    Answer getMaxAligned( const string& input, int row=0, int col=0,
        int minUnaligned=numeric_limits<int>::max(), int minTick=0,
        int minRow=0, int maxRow=0, int minCol=0, int maxCol=0,
        Lights lights={}, Lights alignedLights={}, Aligned unaligned={},
        Answer ans={} ) const noexcept
    {
        regex pattern( "^position=<\\s*(-?\\d+),\\s*(-?\\d+)\\s*>\\s*velocity=<\\s*(-?\\d+),\\s*(-?\\d+)\\s*>$" );
        smatch group;
        istringstream inputStream{ input };
        for( string line; getline( inputStream, line ); )
        {
            if( regex_match( line, group, pattern ) && group.size() == 5 )
            {
                stringstream parser; parser << group[ 1 ] << ' ' << group[ 2 ] << ' ' << group[ 3 ] << ' ' << group[ 4 ];
                parser >> col >> row; Position pos{ row, col };
                parser >> col >> row; Velocity speed{ row, col };
                lights.emplace_back( std::move( Light{pos,speed} ) );
            }
        }

        for( auto tick{ 0 }; tick < 20000; ++tick )
        {
            auto uniqueTotal{ 0 }; Unique uniqueRow, uniqueCol;
            for( const auto& light: lights )
                uniqueRow.insert( light.pos.row ),
                uniqueCol.insert( light.pos.col );
            unaligned.push_back( uniqueRow.size() + uniqueCol.size() );
            int unalignedCount = *min_element( unaligned.cbegin(), unaligned.cend() );
            if( minUnaligned > unalignedCount )
            {
                minUnaligned = unalignedCount;
                ans.lights = lights;
                ans.tick = tick;
                auto[ minRowIt, maxRowIt ] = minmax_element( uniqueRow.cbegin(), uniqueRow.cend() );
                auto[ minColIt, maxColIt ] = minmax_element( uniqueCol.cbegin(), uniqueCol.cend() );
                ans.minRow = *minRowIt, ans.maxRow = *maxRowIt;
                ans.minCol = *minColIt, ans.maxCol = *maxColIt;
            }

            for( auto& light: lights )
                light.pos.row += light.speed.row,
                light.pos.col += light.speed.col;
        }

        return ans;
    }
};


int main()
{
    Solution s;
    auto ans{ s.getMaxAligned( INPUT ) };
    Message message;
    for( const auto& light: ans.lights )
        message.insert( light.pos );

    cout << endl << endl << "Wait for " << ans.tick << " seconds:" << endl << endl;
    for( auto i{ ans.minRow }; i <= ans.maxRow; ++i, cout << endl )
        for( auto j{ ans.minCol }; j <= ans.maxCol; ++j )
                cout << ( ( message.find( { i, j } ) == message.end() )? '.' : '#' );

/*

#...#..###
#...#...#.
#...#...#.
#####...#.
#...#...#.
#...#...#.
#...#...#.
#...#..###

*/

/*

Wait for 10605 seconds:

#####...#....#.....###..#....#.....###....##....######..#....#
#....#..#....#......#...#....#......#....#..#...#.......#....#
#....#...#..#.......#....#..#.......#...#....#..#........#..#.
#....#...#..#.......#....#..#.......#...#....#..#........#..#.
#####.....##........#.....##........#...#....#..#####.....##..
#....#....##........#.....##........#...######..#.........##..
#....#...#..#.......#....#..#.......#...#....#..#........#..#.
#....#...#..#...#...#....#..#...#...#...#....#..#........#..#.
#....#..#....#..#...#...#....#..#...#...#....#..#.......#....#
#####...#....#...###....#....#...###....#....#..######..#....#

*/

    return 0;
}
