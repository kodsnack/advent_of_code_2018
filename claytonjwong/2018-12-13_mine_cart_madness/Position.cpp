//
// Created by Clayton Wong on 2018-12-13.
//

#include "Position.hpp"


std::ostream& operator<<( std::ostream& stream, const Position& position ) noexcept
{
    stream << "(" << position.x << "," << position.y << ")";
    return stream;
}


bool Position::operator==( const Position& rhs ) const noexcept
{
    return ( x == rhs.x ) && ( y == rhs.y );
}
