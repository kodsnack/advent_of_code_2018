//
// Created by Clayton Wong on 2018-12-16.
//

#include <iostream>
#include <vector>


using namespace std;
using VI = vector< int >; using VVI = vector< VI >;
struct Answer { int power{ 0 }, row{ 0 }, col{ 0 }, size{ 0 }; };


const int N{ 301 }, SERIAL_NUMBER{ 8444 };


struct Solution
{
    Answer getMaxPower( Answer ans={} ) const noexcept
    {
        VVI G( N, VI( N, 0 ) );
        for( int row{ 1 }; row < N; ++row ) for( int col{ 1 }; col < N; ++col )
            G[ row ][ col ] = ( ( ( ( ( col + 10 ) * row ) + SERIAL_NUMBER ) * ( col + 10 ) ) / 100 % 10 ) - 5;
        for( int row{ 1 }; row < N; ++row ) for( int col{ 1 }; col < N; ++col )
            for( int size{ 2 }, power{ 0 }; size < N; ++size, power = 0 ) if( row < N - size && col < N - size )
                for( int i{ row }; i <= row + size; ++i )
                    for( int j{ col }; j <= col + size; ++j, ans = ( ans.power < power )? Answer{ power, row, col, size + 1 } : ans )
                        power += G[ i ][ j ];
        return ans;
    }
};


int main()
{
    Solution s;
    auto result = s.getMaxPower();
    cout << "( " << result.col << ", " << result.row << " ), size: " << result.size << endl;

    return 0;
}
