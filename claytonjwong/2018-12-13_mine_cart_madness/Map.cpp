//
// Created by Clayton Wong on 2018-12-13.
//

#include "Map.hpp"
#include <string>
#include <utility>


using namespace std;


Map::Map( const string& input, string line ) :
    map_{},
    width_{ 0 },
    height_{ 0 }
{
    istringstream stream{ input };
    for( ; getline( stream, line ); ++height_ )
        width_ = ( 0 < width_ )? width_ : line.size(),
            map_.emplace_back( std::move( line ) );
}


vector< string >::iterator Map::begin() noexcept
{
    return map_.begin();
}


vector< string >::iterator Map::end() noexcept
{
    return map_.end();
}


vector< string >::const_iterator Map::begin() const noexcept
{
    return map_.cbegin();
}


vector< string >::const_iterator Map::end() const noexcept
{
    return map_.cend();
}


size_t Map::size() const noexcept
{
    return map_.size();
}


char Map::get( const Position& pos ) const noexcept
{
    return map_[ pos.y ][ pos.x ];
}


const string& Map::operator[]( int index ) const noexcept
{
    return map_[ index ];
}


void Map::set( Position&& pos, const char toBeMapped ) noexcept
{
    map_[ pos.y ][ pos.x ] = toBeMapped;
}


ostream& operator<<( ostream& stream, const Map& map ) noexcept
{
    for( const auto& line: map )
        stream << line << endl;
    stream << endl;
    return stream;
}
