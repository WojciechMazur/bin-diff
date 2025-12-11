cmake_minimum_required(VERSION 3.10)
project(bin_compare_demo CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_library(math_utils
    src/math_utils.cpp
)

target_include_directories(math_utils PUBLIC include)

add_library(logger
    src/logger.cpp
)

target_include_directories(logger PUBLIC src)

add_executable(demo
    src/main.cpp
)

target_link_libraries(demo
    PRIVATE
        math_utils
        logger
)

# You can play with flags / warnings to simulate your Yocto setup
target_compile_options(math_utils PRIVATE -Wall -Wextra -Werror)
target_compile_options(logger PRIVATE -Wall -Wextra -Werror)
target_compile_options(demo PRIVATE -Wall -Wextra -Werror)

