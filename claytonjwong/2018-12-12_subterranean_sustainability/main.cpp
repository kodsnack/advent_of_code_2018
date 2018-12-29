//
// Created by Clayton Wong on 2018-12-17.
//

#include "input.hpp"
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <set>
#include <unordered_set>
#include <numeric>


using namespace std;
using Plants = set< int >;
using Grow = unordered_set< string >;


Plants createFirstGeneration( const string& init, Plants firstGen={} )
{
    for( size_t i{ 0 }; i < init.size(); ++i )
        if( init[ i ] == '#' )
            firstGen.insert( i );
    return firstGen;
}


Grow createGrowingConditions( const string& notes, Grow grow={} )
{
    istringstream stream{ notes };
    for( string line; getline( stream, line ); )
        if( line.back() == '#' )
            grow.insert( line.substr( 0,5 ) );
    return grow;
}


string getSection( const Plants& plants, const int center, string section={} )
{
    for( int j{ center-2 }; j <= center+2; ++j )
        section.push_back( plants.find( j ) == plants.end() ? '.' : '#' );
    return section;
}


Plants generate( const Plants& cur, const Grow& grow, Plants next={} ) noexcept
{
    auto[ L, R ] = minmax_element( cur.cbegin(), cur.cend() );
    for( int i{ *L-2 }; i <= *R+2; ++i )
        if( grow.find( getSection( cur, i ) ) != grow.end() )
            next.insert( i );
    return next;
}


int main()
{
    Plants cur = createFirstGeneration( INITIAL_STATE );
    Grow grow = createGrowingConditions( NOTES );

    for( int generation{ 20 }; generation--;  )
    {
        auto next = generate( cur, grow );
        swap( cur, next );
    }

    cout << "answer part 1: " << accumulate( cur.cbegin(), cur.cend(), 0 ) << endl;

    return 0;
}