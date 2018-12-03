cmake_minimum_required(VERSION 3.10)

set (CMAKE_CXX_STANDARD 17)
set (CMAKE_CXX_STANDARD_REQUIRED TRUE)
set (CMAKE_XCODE_ATTRIBUTE_COMPILER_INDEX_STORE_ENABLE NO)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(Day01)
add_subdirectory(Day02)
add_subdirectory(Day03)
