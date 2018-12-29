//
// Created by Clayton Wong on 2018-12-13.
//

#pragma once


#include "Turn.hpp"
#include "Director.hpp"
#include "Map.hpp"
#include <utility>
#include <iostream>


class Cart
{
    size_t id_{ 0 };
    Position pos_{ 0, 0 };
    Director::Direction dir_{ 0 };
    Turn turn_;
    bool crashed_{ false };

    void updatePosition() noexcept;
    void updateDirection( const Map& map ) noexcept;

public:

    Cart( size_t id, Position&& pos, Director::Direction&& dir ) :
        id_{ id },
        pos_{ std::move( pos ) },
        dir_{ std::move( dir )}
    {
    }
    ~Cart() = default;
    Cart( const Cart& ) = default;
    Cart( Cart&& ) = default;
    Cart& operator=( const Cart& ) = default;
    Cart& operator=( Cart&& ) = default;

    size_t getID() const noexcept;
    Position getPosition() const noexcept;
    Director::Direction getDirection() const noexcept;
    char getLastTurn() const noexcept;
    bool isCrashed() const noexcept;

    void setCrashed() noexcept;
    void advance( const Map& map ) noexcept;

};

bool operator<( const Cart& lhs, const Cart& rhs );

std::ostream& operator<<( std::ostream& stream, const Cart& cart ) noexcept;
