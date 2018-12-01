
include_directories(${CMAKE_CURRENT_BINARY_DIR})

set (DAY "01")

configure_file(config.h.in config.h)

add_executable(Day${DAY} "day${DAY}.cpp" input.txt config.h.in)
