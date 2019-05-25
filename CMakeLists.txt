cmake_minimum_required(VERSION 2.8)
project(bigint)

set(CMAKE_CXX_STANDARD 17)

add_executable(big_integer_testing
        big_integer_testing.cpp

        gtest/gtest-all.cc
        gtest/gtest.h
        gtest/gtest_main.cc
        big_integer.h
        big_integer.cpp)

add_executable(big_integer_main
        main.cpp
        big_integer.h
        big_integer.cpp)

if(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ggdb -O2 -Wall -std=c++11 -pedantic")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall -fsanitize=address,undefined,leak -D_GLIBCXX_DEBUG")
endif()

target_link_libraries(big_integer_testing -lgmp -lgmpxx -lpthread)