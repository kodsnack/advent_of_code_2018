//
// Created by Clayton Wong on 2018-12-15.
//

#include "input.hpp"
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <numeric>
#include <queue>


using namespace std;
using VI = vector< int >;
struct Node
{
    int childCount{ 0 }, dataCount{ 0 }, value{ 0 };
    VI childrenValues{ 0 };
};
using Stack = vector< Node >;
using DI = deque< int >;
using PII = pair< int, int >;


class Solution
{
public:

    PII getSumMetadata( const string& input, bool hasChild=false, int childCount=0, int dataCount=0, int sum=0,
                        Node cur={}, DI q={}, Stack stack={}, VI metadata={} ) const noexcept
    {
        istringstream inputStream{ input };
        for( string line, token; getline( inputStream, line ); )
        {
            istringstream lineStream{ line };
            for( int num{ 0 }; getline( lineStream, token, ' '); q.push_back( num ) )
            {
                istringstream parser{ token };
                parser >> num;
            }
        }

        while( ! q.empty() )
            if( ! stack.empty() && stack.back().childCount == 0 )
            {
                for( cur = stack.back(), stack.pop_back(); cur.dataCount--; metadata.push_back( q.front() ), q.pop_front() )
                    if( cur.childrenValues.size() == 1 )
                        cur.value += q.front();
                    else if( q.front() < cur.childrenValues.size() )
                        cur.value += cur.childrenValues[ q.front() ];

                if( ! stack.empty() )
                    --stack.back().childCount,
                    stack.back().childrenValues.push_back( cur.value );
            }
            else
                childCount = q.front(), q.pop_front(),
                dataCount = q.front(), q.pop_front(),
                stack.emplace_back( Node{ childCount, dataCount } );

        return { accumulate( metadata.begin(), metadata.end(), 0 ), cur.value };
    }
};


int main()
{
    Solution s;
    auto result = s.getSumMetadata( INPUT );
    cout << "first: " << result.first << " second: " << result.second << endl;

    return 0;
}
