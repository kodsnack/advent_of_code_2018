//
// Created by Clayton Wong on 2018-12-14.
//

#include "input.hpp"
#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <set>


using namespace std;
using VI = vector< int >;
using VS = vector< string >;


class Solution
{
public:
    int getChecksum( const string& input, int doubleCount=0, int tripleCount=0 )
    {
        istringstream stream{ input };
        for( string line; getline( stream, line ); )
        {
            VI counter( 123, 0 );
            for( const auto character: line )
                ++counter[ character ];
            if( find( counter.cbegin(), counter.cend(), 2 ) != counter.end() ) ++doubleCount;
            if( find( counter.cbegin(), counter.cend(), 3 ) != counter.end() ) ++tripleCount;
        }
        return doubleCount * tripleCount;
    }

    string getOffByOne( const string& input, VS lines={}, string ans={} )
    {
        istringstream stream{ input };
        for( string line; getline( stream, line ); lines.emplace_back( std::move(line) ) );
        sort( lines.begin(), lines.end() );
        for( auto pre{ lines.cbegin() }, cur{ next(pre) }; cur != lines.cend(); pre=cur, cur=next(pre) )
        {
            auto[ preIt, curIt ] = mismatch( pre->cbegin(), pre->cend(), cur->cbegin() ); // assume preIt/curIt != end()
            auto[ endPreIt, endCurIt ] = mismatch( preIt + 1, pre->cend(), curIt + 1 );

            if( endPreIt == pre->end() && endCurIt == cur->end() ) // true if one mismatch was found
            {
                ans = *cur, ans.erase( distance( cur->begin(), curIt ), 1 ); // erase the first mismatched character
                break;
            }
        }
        return { ans.cbegin(), ans.cend() };
    }
};


int main()
{
    Solution s;
    cout << s.getChecksum( INPUT ) << endl;
    cout << s.getOffByOne( INPUT ) << endl;

    return 0;
}
