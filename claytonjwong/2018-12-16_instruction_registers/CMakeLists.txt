cmake_minimum_required(VERSION 3.12)
project(2018_12_16_instruction_registers)

set(CMAKE_CXX_STANDARD 17)

add_executable(2018_12_16_instruction_registers main.cpp input.hpp Command.cpp Command.hpp Register.cpp Register.hpp CommandSuite.hpp CommandSuite.cpp)