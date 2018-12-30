cmake_minimum_required(VERSION 3.12)

project(2018_12_13_mine_cart_madness)

set(CMAKE_CXX_STANDARD 17)

add_executable(2018_12_13_mine_cart_madness Controller.cpp Cart.cpp Cart.hpp Map.cpp Map.hpp Input.hpp Director.cpp Director.hpp Model.cpp Model.hpp Position.cpp Position.hpp Turn.cpp Turn.hpp)
