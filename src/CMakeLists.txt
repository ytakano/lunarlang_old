IF(POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
ENDIF(POLICY CMP0042)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "release")
endif()

# build options
set(CMAKE_CXX_FLAGS "-Wall -fPIC -std=c++14")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")

FILE(GLOB CPPSources *.cpp)
FILE(GLOB CPPHeaders *.hpp)
FILE(GLOB CPPMain lunar_main.cpp)

message(STATUS "CMAKE_BUILD_TYPE=" ${CMAKE_BUILD_TYPE})

add_library(lunarlang SHARED ${CPPSources})
add_executable(lunar ${CPPMain})