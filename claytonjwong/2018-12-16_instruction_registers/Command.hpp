//
// Created by Clayton Wong on 2018-12-17.
//

#pragma once


#include "Register.hpp"
#include <memory>
#include <unordered_set>


class Command
{
public:

    const int opcode{ 0 }, A{ 0 }, B{ 0 }, C{ 0 };

    Command( const int op, const int a, const int b, const int c ) noexcept;
    virtual Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept;
    virtual std::string getName() const noexcept;
};


class AddRegister : public Command // stores into register C the result of adding register A and register B
{
public:
    AddRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class AddImmediate : public Command // stores into register C the result of adding register A and value B
{
public:
    AddImmediate( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class MultiplyRegister : public Command // stores into register C the result of multiplying register A and register B
{
public:
    MultiplyRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class MultiplyImmediate : public Command // stores into register C the result of multiplying register A and value B
{
public:
    MultiplyImmediate( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class BitwiseAndRegister : public Command // stores into register C the result of the bitwise AND of register A and register B
{
public:
    BitwiseAndRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class BitwiseAndImmediate : public Command // stores into register C the result of the bitwise AND of register A and value B
{
public:
    BitwiseAndImmediate( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class BitwiseOrRegister : public Command // stores into register C the result of the bitwise OR of register A and register B
{
public:
    BitwiseOrRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class BitwiseOrImmediate : public Command // stores into register C the result of the bitwise OR of register A and value B
{
public:
    BitwiseOrImmediate( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class SetRegister : public Command // copies the contents of register A into register C. (Input B is ignored.)
{
public:
    SetRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class SetImmediate : public Command // stores value A into register C. (Input B is ignored.)
{
public:
    SetImmediate( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class GreaterImmediateRegister : public Command // sets register C to 1 if value A is greater than register B.
{                                                   // Otherwise, register C is set to 0
public:
    GreaterImmediateRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class GreaterRegisterImmediate : public Command // sets register C to 1 if register A is greater than value B
{                                                   // Otherwise, register C is set to 0
public:
    GreaterRegisterImmediate( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class GreaterRegisterRegister : public Command // sets register C to 1 if register A is greater than register B
{                                                   // Otherwise, register C is set to 0.
public:
    GreaterRegisterRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class EqualImmediateRegister : public Command // sets register C to 1 if value A is equal to register B.
{                                             // Otherwise, register C is set to 0
public:
    EqualImmediateRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class EqualRegisterImmediate : public Command // sets register C to 1 if register A is equal to value B
{                                             // Otherwise, register C is set to 0
public:
    EqualRegisterImmediate( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};


class EqualRegisterRegister : public Command // sets register C to 1 if register A is equal to register B
{                                                   // Otherwise, register C is set to 0.
public:
    EqualRegisterRegister( const int op, const int a, const int b, const int c );
    Register execute( const Register& input, const int a=-1, const int b=-1, const int c=-1 ) const noexcept override;
    std::string getName() const noexcept override;
};
