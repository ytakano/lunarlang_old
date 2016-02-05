cmake_minimum_required (VERSION 2.8)
project (TSUKUYOMI)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

set (TSUKUYOMI_VERSION_MAJOR 0)
set (TSUKUYOMI_VERSION_MINOR 1)

add_subdirectory(src)