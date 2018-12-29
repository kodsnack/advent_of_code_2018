//
// Created by Clayton Wong on 2018-12-13.
//

#pragma once


#include <vector>
#include <string>


class Director
{
    const static std::string mapping_;
    const static std::vector< std::string > stateMachine_;

public:

    Director() = delete;
    ~Director() = delete;
    Director( const Director& ) = delete;
    Director( Director&& ) = delete;
    Director& operator=( const Director& ) = delete;
    Director& operator=( Director&& ) = delete;

    enum class Direction{ up = 0, right = 1, down = 2, left = 3 };

    const static char UP{ '^' }, RIGHT{ '>' }, DOWN{ 'v' }, LEFT{ '<' };

    static Direction getNextDirection( const Direction& dir, char turnKey ) noexcept;

    static char to_char( const Direction& dir ) noexcept;
};

std::ostream& operator<<( std::ostream& stream, const Director::Direction& dir ) noexcept;
