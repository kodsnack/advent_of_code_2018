//
// Created by Clayton Wong on 2018-12-15.
//

#include "input.hpp"
#include <iostream>
#include <regex>
#include <string>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <queue>


using namespace std;
using Queue = priority_queue< char, vector< char >, greater< char > >;
using CharSet = unordered_set< char >;
using CharMap = unordered_map< char, CharSet >;


class Solution
{
    regex pattern{ "^Step ([A-Z]) must be finished before step ([A-Z]) can begin.$" };
public:

    string getBuildOrder( const string& input, CharMap G={}, CharSet V={}, Queue q={}, string ans={} ) const noexcept
    {
        istringstream stream{ input };
        for( string line; getline( stream, line ); ) // build (G)raph of parent/child relationships
        {
            smatch group;
            if( regex_match( line, group, pattern ) && group.size() == 3 )
            {
                char parent{ 0 }, child{ 0 };
                stringstream parser; parser << group[ 1 ] << ' ' << group[ 2 ]; parser >> parent >> child;
                if( G.find( child ) == G.end() ) G[ child ] = {};
                if( G.find( parent ) == G.end() ) G[ parent ] = {};
                G[ child ].insert( parent );
            }
        }

        // only push children with no parents onto the queue for BFS
        for( const auto& x: G ) if ( x.second.empty() && V.insert( x.first ).second )
            q.push( x.first );
        while( ! q.empty() )
        {
            ans.push_back( q.top() ), q.pop();
            for( const auto& x: G )
            {
                G[ x.first ].erase( ans.back() );
                if( G[ x.first ].empty() && V.insert( x.first ).second ) q.push( x.first );
            }
        }

        return ans;
    }

};

int main()
{
    Solution s;
    cout << s.getBuildOrder( INPUT ) << endl;

    return 0;
}