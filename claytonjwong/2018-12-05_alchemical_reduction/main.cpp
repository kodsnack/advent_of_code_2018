//
// Created by Clayton Wong on 2018-12-14.
//

#include "input.hpp"
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>


using namespace std;
using VI = vector< int >;


class Solution
{
    bool isReaction( const char a, const char b ) const noexcept
    {
        return tolower( a ) == tolower( b ) && abs( a - b ) == 32; // same letter upper/lowercase ordinals differ by 32
    }

public:

    string performReduction( const string& polymer, VI stack={} ) const noexcept
    {
        for( const auto unit: polymer )
            if( ! stack.empty() && isReaction( unit, stack.back() ) )
                stack.pop_back();
            else
                stack.push_back( unit );
        return { stack.cbegin(), stack.cend() };
    }

    size_t minReductionSize( const string& polymer, size_t minSize=numeric_limits< size_t >::max() ) const noexcept
    {
        for( char X{ 97 }; X < 123; ++X ) // lowercase [a:z] unit of (p)olymer to e(X)clude
        {
            auto p{ polymer };
            p.erase( remove_if( p.begin(), p.end(),
                [=]( const auto unit ){ return tolower( unit )==X; }), p.end() );
            auto result{ performReduction( p ) };
            minSize = min( minSize, result.size() );
        }
        return minSize;
    }
};


int main()
{
    Solution s;
    string result;
    result = s.performReduction( INPUT );
    cout << " size(): " << result.size() << endl;
    cout << " minSize(): " << s.minReductionSize( INPUT ) << endl;

    return 0;
}
