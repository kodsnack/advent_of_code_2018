cmake_minimum_required(VERSION 3.10)
project(aoc18_f00ale)

set(CMAKE_CXX_STANDARD 17)

add_compile_options($<$<BOOL:${SANITIZE}>:-fsanitize=address,leak>)
add_link_options($<$<BOOL:${SANITIZE}>:-fsanitize=address,leak>)

add_compile_options(-Wall -Wextra)

add_executable(template src/template.cpp)

add_executable(p01 src/p01.cpp)
add_executable(p02 src/p02.cpp)
add_executable(p03 src/p03.cpp)
add_executable(p04 src/p04.cpp)
add_executable(p05 src/p05.cpp)
add_executable(p06 src/p06.cpp)
add_executable(p07 src/p07.cpp)
add_executable(p08 src/p08.cpp)
add_executable(p09 src/p09.cpp)
add_executable(p10 src/p10.cpp)
add_executable(p11 src/p11.cpp)
add_executable(p12 src/p12.cpp)
add_executable(p13 src/p13.cpp)
add_executable(p14 src/p14.cpp)
add_executable(p15 src/p15.cpp)
add_executable(p16 src/p16.cpp)
