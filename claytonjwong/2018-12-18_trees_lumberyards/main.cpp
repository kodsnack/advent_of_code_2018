//
// Created by Clayton Wong on 2018-12-17.
//

#include <iostream>
#include <string>
#include <vector>
#include <sstream>


using namespace std;
using VI = vector< int >;
using VVI = vector< vector< int > >;
using VC = vector< char >;
using VVC = vector< VC >;

/*
const int N{ 10 };
const string input =
R"(.#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.
)";
*/

const int N{ 50 };
const string input =
R"(#...|.|.|....##.##....|.#...|.|.|......#....|#...#
#..#|.#...||#...|.#..#...#......|.|#..#.##....#|#.
.#..##...#|.|..#|....|..#|.|#.#|.......#..#...|#..
.|.|#..|.#....##......#..#.#...|....|#...|#.......
#....|#.#.|..|..#.....#|.#.||.#.|.....#..#..#.....
.#||##..#..|.|....#.|....#.#..|.....#.#|........|.
.|###|#.|..##|#|...#|||.|..|.|.|#|.#.#|.#...#.|.##
..||#..##.||..|...|..|.||.|#...|..|..#....###.#...
..|......##.....#.###......#.#.......|...|.#|...#|
#..|...|.|#|.|....|#.......|.#.....#|.#.....#..#.#
.#.|..#..#|##..|||..##............#..|..#..|...|.#
.##.#|....|..#..|.#.|....|.||#...|..#||......|#...
.....#....#.#.#.#.||.....#.##|..#...#.|......#....
##.#|.#...#.|..|.|....#|#.....#.|.###|#....#|||...
......|..#......#..|...|..#|##..#|.......||.|....|
..#......#.|.|#....##.##.#.|.|..|##..###....#||.|#
.#|.....#...#..#.||.#||.##...||....|.......|#...##
.......||..|#.|##...#.|.#..|..|.#.|####|.||...|#|.
#...#.#..|##..|#..|.#.|#.|##..|..|#....#.|.....#..
|....#..#.....|..##..#..|.|#..||#|#...||..#|..|...
........##......|......|......||.......##.|#.||..|
|..#.||.#.#..#..|||....|#|..|.#|...|..|.#.||.|.|.|
.#...|..|.|#.......|#.|......|...#|||....#..|...|#
...###|...#..|..#.|...|#|.....##.....|.##.||.|.##|
#..##.|.||#.#....#|.....|#..||...#.||.##|.#..|..|.
|....|#.....|...#..|..|..#..#|........||..#.|.....
..#......|..#....#.|....#.|###|.##|.|...#.#...||..
..#.|#....|...#||.|.#...#..#...#..#.|.#|.||..|#..|
....|.|#..|...||..||#.|..#||.|#..#.#....#.|.....#|
#|.|..|..#......|#|||.##.#......#.|#..#.|.##.#|..#
..#|......##.|.#...|.#...#....#.###|#......|......
..#.#.#|...#|....|...#.##...##...#..#|..|#.|###...
|||.#..###.|......|..#.....#...|#.|.|...#...#|..#|
##...#.#|#.|....||....|.....#|....#....|..|....#.#
.........###.#.#..##..|##..#|...#...|...#|###.|..|
.|#.........##.....#.##.|#.#....#....#|....#....#.
.|....#...#||.|.......|.#..#.|..|#...#|.....|...#.
.|#..|||....#..|#|.|.#.||..|.#|.#|#|||.|#||...#.#.
#.#|.|.#...#.#.|||..|...#..#.##....#.#.#|.||....##
.|.|....##..|#|.#...|#.|.|...#..#|#....|#.|.##.##.
...#....#.....#..#...#..#.|#..#.|#.|..#..#.....|..
#...#..|..###|....#.|...##|...#|...#.#....##...#.#
.#.##..#.#.......................|..#|..##.|.|....
|..#|#..|#....#...#....|..##..#||#..#.#.#.|#|.||##
#.#...#.#...##.||...||....#...##|#|....|||#..|.|.|
....#....|.|.....#|#...#..#|#....|#.|.#|.|..#....|
|#.......||.#.|..|......##.||.....||.|..|....||#..
..|#.....|...##.##..#|##|#....####..#|.......#.|..
||#.#.|.##..#.|....#.||###..####||.#||...##.#..#|#
.|....#.....#.....##.#..|#...|||.|....|#..|#|...#.
)";




const VVI adj{ {-1,0}, {-1,1}, {0,1}, {1,1}, {1,0}, {1,-1}, {0,-1}, {-1,-1} }; // clockwise relative adjacent cells

bool isAdj( const VVC& G, const int row, const int col, const char target, const int threshold )
{
    int found{ 0 };
    for( const auto a: adj)
    {
        auto r{ row + a[ 0 ] }, c{ col + a[ 1 ] };
        if( 0 <= r && r < N  &&  0 <= c && c < N )
            if( G[ r ][ c ] == target )
                ++found;
    }
    return found >= threshold;
}

int main()
{
    VVC cur, next( N, VC( N, '.' ) );
    istringstream stream{ input };
    for( string line; getline( stream, line ); )
    {
        cur.push_back( {} );
        for( const char c: line )
            cur.back().push_back( c );
    }

    for( auto m{ 10 }; m--; cur.swap( next ) ) for( auto r{ 0 }; r < N; ++r ) for( auto c{ 0 }; c < N; ++c )
        if( cur[ r ][ c ] == '.' && isAdj( cur, r, c, '|', 3 ) )
            next[ r ][ c ] = '|';
        else if( cur[ r ][ c ] == '|' && isAdj( cur, r, c, '#', 3 ) )
            next[ r ][ c ] = '#';
        else if( cur[ r ][ c ] == '#' )
            next[ r ][ c ] = ( isAdj( cur, r, c, '|', 1 ) && isAdj( cur, r, c, '#', 1 ) )? '#' : '.';
        else
            next[ r ][ c ] = cur[ r ][ c ];

    int T{ 0 }, L{ 0 }; // count of (T)rees / (L)umberyards
    for( const auto row: cur )
        T += count_if( row.cbegin(), row.cend(), []( auto c ){ return c == '|'; }),
        L += count_if( row.cbegin(), row.cend(), []( auto c ){ return c == '#'; });
    cout << "answer part 1: " << T * L << endl;

    return 0;
}