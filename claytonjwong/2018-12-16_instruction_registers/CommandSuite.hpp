//
// Created by Clayton Wong on 2018-12-17.
//

#pragma once


#include "Command.hpp"
#include <vector>
#include <memory>
#include <unordered_set>


using CommandSet = std::vector< std::shared_ptr< Command > >; // opcode is the index [0:15] ( size() == 16 )
using Iter = std::vector< std::shared_ptr< Command > >::const_iterator;


class CommandSuite
{
    CommandSet commands_;

    void Init( const int op, const int a, const int b, const int c ) noexcept;

public:

    CommandSuite() noexcept;
    CommandSuite( const int op, const int a, const int b, const int c ) noexcept;

    Register Execute( const Register& input, const int op, const int a, const int b, const int c ) const noexcept;

    Iter begin();
    Iter end();
};
