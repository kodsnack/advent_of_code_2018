//
// Created by Clayton Wong on 2018-12-17.
//

#include "CommandSuite.hpp"
#include <functional>


using namespace std;


void CommandSuite::Init( const int op, const int a, const int b, const int c ) noexcept
{
    //
    // the collection index is the value of the derived opcode
    //
    // ( the parameter op is an opcode which may be ambiguous from part 1 of this question's sample input  )
    //
    // ( I derived the actual opcodes via deductive reasoning by comparing the non-ambiguous remaining unassigned opcodes, one-by-one )
    //
    commands_.emplace_back( make_shared< GreaterImmediateRegister >( op,a,b,c) );   // 00
    commands_.emplace_back( make_shared< MultiplyRegister >( op,a,b,c ) );          // 01
    commands_.emplace_back( make_shared< SetImmediate >( op,a,b,c) );               // 02
    commands_.emplace_back( make_shared< GreaterRegisterRegister >( op,a,b,c) );    // 03
    commands_.emplace_back( make_shared< BitwiseOrImmediate >( op,a,b,c) );         // 04
    commands_.emplace_back( make_shared< BitwiseOrRegister >( op,a,b,c) );          // 05
    commands_.emplace_back( make_shared< BitwiseAndRegister >( op,a,b,c) );         // 06
    commands_.emplace_back( make_shared< EqualRegisterImmediate >( op,a,b,c) );     // 07
    commands_.emplace_back( make_shared< BitwiseAndImmediate >( op,a,b,c) );        // 08
    commands_.emplace_back( make_shared< AddRegister >( op,a,b,c ) );               // 09
    commands_.emplace_back( make_shared< AddImmediate >( op,a,b,c ) );              // 10
    commands_.emplace_back( make_shared< EqualRegisterRegister >( op,a,b,c) );      // 11
    commands_.emplace_back( make_shared< GreaterRegisterImmediate >( op,a,b,c) );   // 12
    commands_.emplace_back( make_shared< EqualImmediateRegister >( op,a,b,c) );     // 13
    commands_.emplace_back( make_shared< SetRegister >( op,a,b,c) );                // 14
    commands_.emplace_back( make_shared< MultiplyImmediate >( op,a,b,c) );          // 15
}


CommandSuite::CommandSuite() noexcept
{
    Init( 0,0,0,0 );
}


CommandSuite::CommandSuite( const int op, const int a, const int b, const int c ) noexcept
{
    Init( op,a,b,c );
}

Iter CommandSuite::begin(){ return commands_.begin(); }
Iter CommandSuite::end(){ return commands_.end(); }

Register CommandSuite::Execute( const Register& input, const int op, const int a, const int b, const int c ) const noexcept
{
    return commands_[ op ]->execute( input, a, b, c );
}
