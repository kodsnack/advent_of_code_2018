//
// Created by Clayton Wong on 2018-12-16.
//

#include <iostream>
#include <vector>
#include <queue>
#include <sstream>


using namespace std;
using Score = vector< int >;
using Recipe = deque< int >;


const int PRE{ 554401 }, CUR{ 10 }, TOTAL{ PRE + CUR }; // input values


class Solution
{
public:

    Score getRecipes( Score S={ 3,7 }, size_t i=0, size_t j=1 ) const noexcept
    {
        for( Recipe next; S.size() < TOTAL; next.clear(), i = (i+S[i]+1) % S.size(), j = (j+S[j]+1) % S.size() )
        {
            for( auto score( S[i] + S[j] ); score > 0; score /= 10 ){ next.push_front( score % 10 ); }
            if( next.empty() ){ next.push_back( 0 ); } // 0 + 0 == 0
            std::move( next.cbegin(), next.cend(), back_inserter( S ) );
        }

        Score result{ S.cbegin() + PRE, S.cbegin() + TOTAL + 1 };
        result.resize( CUR );
        return result;
    }

    size_t numRecipesBefore( const string& needle, string haystack="37", Score S={ 3,7 }, size_t i=0, size_t j=1 ) const noexcept
    {
        for( Recipe next;; next.clear(), i = (i+S[i]+1) % S.size(), j = (j+S[j]+1) % S.size() )
        {
            for( auto score( S[i] + S[j] ); score > 0; score /= 10 ){ next.push_front( score % 10 ); }
            if( next.empty() ){ next.push_back( 0 ); } // 0 + 0 == 0

            for( const auto x: next ){ haystack.push_back( '0' + x ); }
            if( haystack.size() + 2 > needle.size() && needle.size() > 2 ) // we just added either 1 or 2 digits onto the haystack
            {                                                              // so check haystack for needle before those two positions
                const auto pos{ haystack.find( needle, haystack.size() - needle.size() - 2 ) };
                if( pos != string::npos ) return pos;
            }
            std::move( next.cbegin(), next.cend(), back_inserter( S ) );
        }
    }
};


ostream& operator<<( ostream& os, const Score& score )
{
    string output; for( const auto digit: score ){ output.push_back( '0' + digit ); }
    os << output;
    return os;
}


int main()
{
    Solution s;
    cout << "recipes: " << s.getRecipes() << endl;

    string target{ to_string( PRE ) };
    cout << "before (" << target << "): " << s.numRecipesBefore( target ) << endl;

    return 0;
}