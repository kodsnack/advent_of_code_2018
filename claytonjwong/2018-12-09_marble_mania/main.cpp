//
// Created by Clayton Wong on 2018-12-15.
//

#include <iostream>
#include <list>
#include <vector>
#include <algorithm>


const int P{ 459 }, M1{ 72103 + 1 }, M2{ 7210300 + 1 }; // # of (P)layers / (M)arbles ( part 1 + part 2 )


using namespace std;
using Score = vector< size_t >;
using Circle = list< int >;


class Solution
{

public:

    size_t getHighScore( const int marbles, Circle circle={ 0 }, Score score=Score( P, 0 ), int player=0 ) const noexcept
    {
        auto it{ circle.begin() };
        for( auto marble{ 1 }; marble < marbles; ++marble, player = ( player + 1 == P )? 0 : player + 1 )
            if( marble % 23 == 0 )
            {
                for( auto moves{ 7 }; moves--; it = ( it == circle.begin() )? prev( circle.end() ) : prev( it ) );
                score[ player ] += marble + *it;
                it = circle.erase( it );
            }
            else
            {
                for( auto moves{ 2 }; moves;  )
                    ++it, it = ( --moves > 0 && it == circle.end() )? it = circle.begin() : it;
                it = circle.insert( it, marble );
            }
        return *max_element( score.cbegin(), score.cend() );
    }
};

int main()
{
    Solution s;
    cout << "first: " << s.getHighScore( M1 ) << " second: " << s.getHighScore( M2 ) << endl;

    return 0;
}
