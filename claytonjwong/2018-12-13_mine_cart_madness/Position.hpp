//
// Created by Clayton Wong on 2018-12-13.
//

#pragma once


#include <iostream>
#include <numeric>


struct Position
{
    size_t x{ 0 }, y{ 0 };

    bool operator==( const Position& rhs ) const noexcept;
};

struct PositionHash
{
    std::size_t operator()(const Position& position ) const
    {
        return ( position.x * std::numeric_limits< short >::max() ) + position.y;
    }
};

std::ostream& operator<<( std::ostream& stream, const Position& position ) noexcept;
