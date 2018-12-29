//
// Created by Clayton Wong on 2018-12-14.
//

#include "input.hpp"
#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <regex>
#include <map>
#include <unordered_map>


using namespace std;
using Timestamps = vector< string >;
using TimeTable =  unordered_map< int, map< int, int > >;
using TimeSums = unordered_map< int, int >;
using PII = pair< int, int >;


class Solution
{
public:

    PII getMaxSleep( const string& input, Timestamps timestamps={}, TimeSums timeSums={}, TimeTable timeTables={},
                     int id=0, int maxID=0, int minute=0, int maxMinute=0, int lastSleep=0, PII ans={} )
    {
        istringstream stream{ input };
        for( string line; getline( stream, line ); timestamps.emplace_back( std::move( line ) ) );
        sort( timestamps.begin(), timestamps.end() );
        regex
            beginPattern( "^\\[\\d+-\\d+-\\d+ \\d+:\\d+\\] Guard #(\\d+) begins shift$" ),
            sleepPattern( "^\\[\\d+-\\d+-\\d+ \\d+:(\\d+)\\] falls asleep$" ),
            wakePattern( "^\\[\\d+-\\d+-\\d+ \\d+:(\\d+)\\] wakes up$" );
        for( const auto& timestamp: timestamps )
        {
            smatch group;
            stringstream stream;
            if ( regex_match( timestamp, group, beginPattern ) && group.size() == 2 )
            {
                stream << group[ 1 ]; stream >> id;
                if ( timeTables.find( id ) == timeTables.end() )
                    timeTables[ id ] = {};
            }
            else if ( regex_match( timestamp, group, sleepPattern ) && group.size() == 2 )
            {
                stream << group[ 1 ]; stream >> minute;
                timeTables[ id ].insert({ minute, 1 }); // +1 to track max overlap in next for-loop below
                lastSleep = minute;
            }
            else if ( regex_match( timestamp, group, wakePattern ) && group.size() == 2 )
            {
                stream << group[ 1 ]; stream >> minute;
                timeTables[ id ].insert({ minute, -1 }); // -1 to track max overlap in next for-loop below
                timeSums[ id ] += minute - lastSleep;
                if ( maxMinute < timeSums[ id ] )
                {
                    maxMinute = timeSums[ id ];
                    maxID = id;
                }
            }
        }

        int overlap{ 0 }, maxOverlap{ 0 };
        for( const auto& times: timeTables[ maxID ] )
        {
            overlap += times.second;
            if ( maxOverlap < overlap )
            {
                maxOverlap = overlap;
                maxMinute = times.first;
            }
        }
        ans.first = maxID * maxMinute;

        maxOverlap = maxMinute = maxID = 0;
        for( const auto& timeTable: timeTables )
        {
            overlap = 0;
            id = timeTable.first;
            for( const auto& times: timeTable.second )
            {
                minute = times.first;
                overlap += times.second;
                if ( maxOverlap < overlap )
                {
                    maxOverlap = overlap;
                    maxMinute = minute;
                    maxID = id;
                }
            }
        }
        ans.second = maxID * maxMinute; // TODO: wrong answer for second part

        return ans;
    }

};


int main() {

    Solution s;
    auto result{ s.getMaxSleep( INPUT ) };
    cout << "first: " << result.first << " second: " << result.second << endl;

    return 0;
}