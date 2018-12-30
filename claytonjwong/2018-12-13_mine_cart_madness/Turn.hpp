//
// Created by Clayton Wong on 2018-12-13.
//

#pragma once


class Turn
{
    int last_{ right };

public:

    const static int left{ 0 }, straight{ 1 }, right{ 2 };

    Turn() = default;
    ~Turn() = default;
    Turn( const Turn& ) = default;
    Turn( Turn&& ) = default;
    Turn& operator=( const Turn& ) = default;
    Turn& operator=( Turn&& ) = default;

    char next() noexcept;
    char last() const noexcept;
};
