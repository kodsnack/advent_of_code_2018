//
// Created by Clayton Wong on 2018-12-14.
//

#include "input.hpp"
#include <iostream>
#include <regex>
#include <set>
#include <unordered_map>
#include <iostream>
#include <sstream>


using namespace std;
using PII = pair< int, int >;


class Solution
{
    struct cmp{
        bool operator()( const PII& lhs, const PII& rhs ) const
        {
            return lhs.first < rhs.first || ( lhs.first == rhs.first && lhs.second < rhs.second );
        }
    };
    using FabricCells = set< PII, cmp >;
    FabricCells visited, duplicate;
    unordered_map< int, FabricCells > claims;

public:

    PII getOverlappingClaims( const string& input, int id=0, int col=0, int row=0, int width=0, int height=0, PII ans={} )
    {                                                                //   1      2     3      4       5
        regex pattern( "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$" ); // #(id) @ (col),(row): (width)x(height)
        istringstream stream{ input };
        for( string line; getline( stream, line ); )
        {
            smatch group;
            if( regex_match( line, group, pattern ) && group.size() == 6 )
            {
                stringstream stream;
                stream << group[ 1 ] << ' ' << group[ 2 ] << ' ' << group[ 3 ] << ' ' << group[ 4 ] << ' ' << group[ 5 ];
                stream >> id >> col >> row >> width >> height;

                claims[ id ] = {};
                for( int i{ row }; i < row + height; ++i )
                    for( int j{ col }; j < col + width; claims[ id ].insert( {i,j} ), ++j )
                        if( ! visited.insert( {i,j} ).second )
                            duplicate.insert( {i,j} );
            }
        }
        ans.first = static_cast< int >( duplicate.size() );

        for( const auto& claim: claims )
        {
            vector< PII > claimDuplicates;
            set_intersection( claim.second.cbegin(), claim.second.cend(),
                              duplicate.cbegin(), duplicate.cend(), back_inserter( claimDuplicates ) );
            if( claimDuplicates.empty() )
                ans.second = claim.first;
        }

        return ans;
    }
};


int main() {

    Solution s;
    auto result{ s.getOverlappingClaims( INPUT ) };
    cout << "overlapping cells (square inches): " << result.first << " unique ID: " << result.second << endl;

    return 0;
}