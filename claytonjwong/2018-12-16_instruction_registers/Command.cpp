//
// Created by Clayton Wong on 2018-12-17.
//

#include "Command.hpp"
#include "Register.hpp"
#include <string>


using namespace std;


Command::Command( const int op, const int a, const int b, const int c ) noexcept : opcode{ op }, A{ a }, B{ b }, C{ c }{ }
Register Command::execute( const Register& input, const int a, const int b, const int c ) const noexcept { return {}; }
string Command::getName() const noexcept { return { "Command" }; }


AddRegister::AddRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register AddRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores into register C the result of adding register A and register B
    if( a>0 && b>0 && c>0 )
         R[ c ] = R[ a ] + R[ b ];
    else
        R[ C ] = R[ A ] + R[ B ];
    return R;
}
string AddRegister::getName() const noexcept { return { "09: AddRegister" }; }


AddImmediate::AddImmediate( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register AddImmediate::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores into register C the result of adding register A and value B
    if( a>0 && b>0 && c>0 )
        R[ c ] = R[ a ] + b;
    else
        R[ C ] = R[ A ] + B;
    return R;
}
string AddImmediate::getName() const noexcept { return { "10: AddImmediate" }; }


MultiplyRegister::MultiplyRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register MultiplyRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores into register C the result of multiplying register A and register B
    if( a>0 && b>0 && c>0 )
        R[ c ] = R[ a ] * R[ b ];
    else
        R[ C ] = R[ A ] * R[ B ];
    return R;
}
string MultiplyRegister::getName() const noexcept { return { "01: MultiplyRegister" }; }


MultiplyImmediate::MultiplyImmediate( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register MultiplyImmediate::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores into register C the result of multiplying register A and value B
    if( a>0 && b>0 && c>0 )
        R[ c ] = R[ a ] * b;
    else
        R[ C ] = R[ A ] * B;
    return R;
}
string MultiplyImmediate::getName() const noexcept { return { "15: MultiplyImmediate" }; }


BitwiseAndRegister::BitwiseAndRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register BitwiseAndRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores into register C the result of the bitwise AND of register A and register B
    if( a>0 && b>0 && c>0 )
        R[ c ] = R[ a ] & R[ b ];
    else
        R[ C ] = R[ A ] & R[ B ];
    return R;
}
string BitwiseAndRegister::getName() const noexcept { return { "06: BitwiseAndRegister" }; }


BitwiseAndImmediate::BitwiseAndImmediate( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register BitwiseAndImmediate::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores into register C the result of the bitwise AND of register A and value B
    if( a>0 && b>0 && c>0 )
        R[ c ] = R[ a ] & b;
    else
        R[ C ] = R[ A ] & B;
    return R;
}
string BitwiseAndImmediate::getName() const noexcept { return { "08: BitwiseAndImmediate" }; }


BitwiseOrRegister::BitwiseOrRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register BitwiseOrRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores into register C the result of the bitwise OR of register A and register B
    if( a>0 && b>0 && c>0 )
        R[ c ] = R[ a ] | R[ b ];
    else
        R[ C ] = R[ A ] | R[ B ];
    return R;
}
string BitwiseOrRegister::getName() const noexcept { return { "05: BitwiseOrRegister" }; }


BitwiseOrImmediate::BitwiseOrImmediate( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register BitwiseOrImmediate::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores into register C the result of the bitwise OR of register A and value B
    if( a>0 && b>0 && c>0 )
        R[ c ] = R[ a ] | b;
    else
        R[ C ] = R[ A ] | B;
    return R;
}
string BitwiseOrImmediate::getName() const noexcept { return { "04: BitwiseOrImmediate" }; }


SetRegister::SetRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register SetRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // copies the contents of register A into register C. (Input B is ignored.)
    if( a>0 && b>0 && c>0 )
        R[ c ] = R[ a ];
    else
        R[ C ] = R[ A ];
    return R;
}
string SetRegister::getName() const noexcept { return { "14: SetRegister" }; }


SetImmediate::SetImmediate( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register SetImmediate::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // stores value A into register C. (Input B is ignored.)
    if( a>0 && b>0 && c>0 )
        R[ c ] = a;
    else
        R[ C ] = A;
    return R;
}
string SetImmediate::getName() const noexcept { return { "02: SetImmediate" }; }


GreaterImmediateRegister::GreaterImmediateRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register GreaterImmediateRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // sets register C to 1 if value A is greater than register B.
    if( a>0 && b>0 && c>0 )
        R[ c ]=( a > R[ b ] )? 1 : 0;
    else
        R[ C ]=( A > R[ B ] )? 1 : 0;
    return R;
}                                                                // Otherwise, register C is set to 0
string GreaterImmediateRegister::getName() const noexcept { return { "00: GreaterImmediateRegister" }; }


GreaterRegisterImmediate::GreaterRegisterImmediate( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register GreaterRegisterImmediate::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // sets register C to 1 if register A is greater than value B, Otherwise, register C is set to 0
    if( a>0 && b>0 && c>0 )
        R[ c ]=( R[ a ] > b )? 1 : 0;
    else
        R[ C ]=( R[ A ] > B )? 1 : 0;
    return R;
}
string GreaterRegisterImmediate::getName() const noexcept { return { "12: GreaterRegisterImmediate" }; }


GreaterRegisterRegister::GreaterRegisterRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register GreaterRegisterRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // sets register C to 1 if register A is greater than register B, Otherwise, register C is set to 0.
    if( a>0 && b>0 && c>0 )
        R[ c ]=( R[ a ] > R[ b ] )? 1 : 0;
    else
        R[ C ]=( R[ A ] > R[ B ] )? 1 : 0;
    return R;
}
string GreaterRegisterRegister::getName() const noexcept { return { "03: GreaterRegisterRegister" }; }


EqualImmediateRegister::EqualImmediateRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register EqualImmediateRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // sets register C to 1 if value A is equal to register B, Otherwise, register C is set to 0
    if( a>0 && b>0 && c>0 )
        R[ c ]=( a == R[ b ] )? 1 : 0;
    else
        R[ C ]=( A == R[ B ] )? 1 : 0;
    return R;
}
string EqualImmediateRegister::getName() const noexcept { return { "13: EqualImmediateRegister" }; }


EqualRegisterImmediate::EqualRegisterImmediate( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register EqualRegisterImmediate::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // sets register C to 1 if register A is equal to value B, Otherwise, register C is set to 0
    if( a>0 && b>0 && c>0 )
        R[ c ]=( R[ a ] == b )? 1 : 0;
    else
        R[ C ]=( R[ A ] == B )? 1 : 0;
    return R;
}
string EqualRegisterImmediate::getName() const noexcept { return { "07: EqualRegisterImmediate" }; }


EqualRegisterRegister::EqualRegisterRegister( const int op, const int a, const int b, const int c ) : Command{ op,a,b,c }{ }
Register EqualRegisterRegister::execute( const Register& input, const int a, const int b, const int c ) const noexcept
{
    Register R{ input }; // sets register C to 1 if register A is equal to register B, Otherwise, register C is set to 0.
    if( a>0 && b>0 && c>0 )
        R[ c ]=( R[ a ] == R[ b ] )? 1 : 0;
    else
        R[ C ]=( R[ A ] == R[ B ] )? 1 : 0;
    return R;
}
string EqualRegisterRegister::getName() const noexcept { return { "11: EqualRegisterRegister" }; }
