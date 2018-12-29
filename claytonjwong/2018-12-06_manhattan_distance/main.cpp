//
// Created by Clayton Wong on 2018-12-15.
//

#include "input.hpp"
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>


using namespace std;


struct Coordinate{ int row{ 0 }, col{ 0 }; };
struct Cell{ int id{ 0 }, minDistance{ numeric_limits< int >::max() }, sumDistance{ 0 }; };


using VC = vector< Coordinate >;
using VI = vector< int >;
using PII = pair< int, int >;
using GridLines = vector< Cell >;
using Grid = vector< GridLines >;
using Counter = unordered_map< int, int >;
using Border = unordered_set< int >;


class Solution
{
public:

    PII getMaxArea( const string& input, string inputWithoutCommas={}, int id=0, int row=0, int col=0, PII ans={},
        VI rows={}, VI cols={}, VC coords={}, Counter counter={}, Border border={ 0 } ) const noexcept
    {
        transform( input.cbegin(), input.cend(), back_inserter( inputWithoutCommas ),  // transform commas to spaces
            [&]( auto c ){ return( c == 44 )? 32 : c; } );
        istringstream inputStream{ inputWithoutCommas };
        for( string line; getline( inputStream, line ); )
        {
            istringstream lineStream{ line }; lineStream >> col >> row;
            coords.emplace_back( Coordinate{ row, col } ), rows.push_back( row ), cols.push_back( col );
        }

        auto[ minRow, maxRow ] = minmax_element( rows.cbegin(), rows.cend() );
        auto[ minCol, maxCol ] = minmax_element( cols.cbegin(), cols.cend() );
        Grid G( *maxRow + 1, GridLines( *maxCol + 1 ) );
        for( const auto& coord: coords )
        {
            G[ coord.row ][ coord.col ].minDistance = 0;
            G[ coord.row ][ coord.col ].id = ++id;
            for( auto row{ *minRow }; row <= *maxRow; ++row ) for( auto col{ *minCol }; col <= *maxCol; ++col )
            {
                auto distance{ abs( coord.row - row ) + abs( coord.col - col ) };
                G[ row ][ col ].sumDistance += distance;
                if( G[ row ][ col ].minDistance > distance )
                    G[ row ][ col ].minDistance = distance,
                    G[ row ][ col ].id = id;
                else
                if( G[ row ][ col ].minDistance == distance && G[ row ][ col ].id != id )
                    G[ row ][ col ].id = 0;
            }
        }

        for( auto row{ *minRow }; row <= *maxRow; ++row ) for( auto col{ *minCol }; col <= *maxCol; ++col )
        {
            id = G[ row ][ col ].id;
            if( row == *minRow || row == *maxRow || col == *minCol || col == *maxCol )
                border.insert( id );
            else
            if( border.find( id ) == border.end() )
                ++counter[ id ];

            if( G[ row ][ col ].sumDistance < 10000 ) ++ans.second;
        }
        ans.first = max_element( counter.cbegin(), counter.cend(),
            []( const PII& lhs, const PII& rhs ){ return lhs.second < rhs.second; })->second;
        return ans;
    }
};


int main()
{
    Solution s;
    auto result = s.getMaxArea( INPUT );
    cout << "first: " << result.first << " second: " << result.second << endl;

    return 0;
}