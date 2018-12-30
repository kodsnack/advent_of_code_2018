//
// Created by Clayton Wong on 2018-12-13.
//

#pragma once


#include "Director.hpp"
#include "Position.hpp"
#include <vector>
#include <string>
#include <iostream>
#include <sstream>


class Map
{
    std::vector< std::string > map_;
    size_t width_{ 0 }, height_{ 0 };

public:

    explicit Map( const std::string& input, std::string line={} );
    ~Map() = default;
    Map( const Map& ) = delete;
    Map( Map&& ) = delete;
    Map& operator=( const Map& ) = delete;
    Map& operator=( Map&& ) = delete;

    std::vector< std::string >::iterator begin() noexcept;
    std::vector< std::string >::iterator end() noexcept;
    std::vector< std::string >::const_iterator begin() const noexcept;
    std::vector< std::string >::const_iterator end() const noexcept;

    size_t size() const noexcept;
    char get( const Position& pos ) const noexcept;
    const std::string& operator[]( int index ) const noexcept;

    void set( Position&& pos, const char toBeMapped ) noexcept;
};

std::ostream& operator<<( std::ostream& stream, const Map& map ) noexcept;
