//
// Created by Clayton Wong on 2018-12-14.
//

#include "input.hpp"
#include <iostream>
#include <vector>
#include <sstream>
#include <unordered_set>


using namespace std;


class Solution
{
public:

    int getFinalFrequency( const string& input, string line={}, char op='\0', int val=0, int freq=0 )
    {
        istringstream inputStream{ INPUT };
        for( ; getline( inputStream, line ); freq = ( op == '+' )? freq + val : freq - val )
        {
            istringstream lineStream{ line };
            lineStream >> op >> val;
        }
        return freq;
    }

    int getDuplicateFrequency( const string& input, string line={}, char op='\0', int val=0, int freq=0, unordered_set<int> unique={ 0 } )
    {
        for( ;; )
            for( istringstream inputStream{ INPUT }; getline( inputStream, line ); )
            {
                istringstream lineStream{ line };
                lineStream >> op >> val;
                freq = ( op == '+' )? freq + val : freq - val;
                if( ! unique.insert( freq ).second )
                    return freq;
            }
    }
};


int main()
{
    Solution s;
    cout << s.getFinalFrequency( INPUT ) << endl;
    cout << s.getDuplicateFrequency( INPUT ) << endl;

    return 0;
}
