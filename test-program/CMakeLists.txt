cmake_minimum_required(VERSION 3.10)
project(bin_compare_demo CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Static libraries (.a)
add_library(math_utils_static STATIC src/math_utils.cpp)
target_include_directories(math_utils_static PUBLIC include)
set_target_properties(math_utils_static PROPERTIES OUTPUT_NAME math_utils)

add_library(logger_static STATIC src/logger.cpp)
target_include_directories(logger_static PUBLIC src)
set_target_properties(logger_static PROPERTIES OUTPUT_NAME logger)

# Shared libraries (.so/.dylib)
add_library(math_utils_shared SHARED src/math_utils.cpp)
target_include_directories(math_utils_shared PUBLIC include)
set_target_properties(math_utils_shared PROPERTIES OUTPUT_NAME math_utils)

add_library(logger_shared SHARED src/logger.cpp)
target_include_directories(logger_shared PUBLIC src)
set_target_properties(logger_shared PROPERTIES OUTPUT_NAME logger)

# String utils library - identical settings to bazel (no defines)
add_library(string_utils_static STATIC src/string_utils.cpp)
target_include_directories(string_utils_static PUBLIC include)
set_target_properties(string_utils_static PROPERTIES OUTPUT_NAME string_utils)

add_library(string_utils_shared SHARED src/string_utils.cpp)
target_include_directories(string_utils_shared PUBLIC include)
set_target_properties(string_utils_shared PROPERTIES OUTPUT_NAME string_utils)

# Executable
add_executable(demo src/main.cpp)
target_link_libraries(demo PRIVATE math_utils_static logger_static)

# Compile options
target_compile_options(math_utils_static PRIVATE -Wall -Wextra -Werror)
target_compile_options(math_utils_shared PRIVATE -Wall -Wextra -Werror)
target_compile_options(logger_static PRIVATE -Wall -Wextra -Werror)
target_compile_options(logger_shared PRIVATE -Wall -Wextra -Werror)
target_compile_options(string_utils_static PRIVATE -Wall -Wextra -Werror)
target_compile_options(string_utils_shared PRIVATE -Wall -Wextra -Werror)
target_compile_options(demo PRIVATE -Wall -Wextra -Werror)

