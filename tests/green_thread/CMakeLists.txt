cmake_minimum_required(VERSION 3.4)

IF(POLICY CMP0042)
  cmake_policy(SET CMP0042 NEW)
ENDIF(POLICY CMP0042)

# for Boost
find_package(Boost COMPONENTS REQUIRED)

# for LLVM
find_package(LLVM REQUIRED CONFIG)
#add_definitions(${LLVM_DEFINITIONS})

# for threading library
find_package(Threads REQUIRED)

include_directories(${LLVM_INCLUDE_DIRS} ${Boost_INCLUDE_DIR})

execute_process(
    COMMAND ${LLVM_TOOLS_BINARY_DIR}/llvm-config --cxxflags
    COMMAND tr -d \n
    OUTPUT_VARIABLE LLVM_CXXFLAGS
)

execute_process(
    COMMAND ${LLVM_TOOLS_BINARY_DIR}/llvm-config --ldflags
    COMMAND tr -d \n
    OUTPUT_VARIABLE LLVM_LDFLAGS
)

# build options
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "release")
endif()

set(LIBS ${LLVM_AVAILABLE_LIBS})
set(CMAKE_CXX_FLAGS "-Wno-gnu-zero-variadic-macro-arguments -fno-rtti ${LLVM_CXXFLAGS} -I../../src")
set(CMAKE_CXX_FLAGS_DEBUG "-g -O0")
set(CMAKE_CXX_FLAGS_RELEASE "-O3")
set(CMAKE_EXE_LINKER_FLAGS ${LLVM_LDFLAGS})

# print status
message(STATUS "Build type: -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}")
message(STATUS "Using LLVMConfig.cmake in: -DLLVM_DIR=${LLVM_DIR}")
message(STATUS "LLVM version: ${LLVM_PACKAGE_VERSION}")
message(STATUS "LLVM C++ compiler flags: ${LLVM_CXXFLAGS}")
message(STATUS "LLVM linker flags: ${LLVM_LDFLAGS}")

# build targets
add_executable(green_thread_cpp green_thread_cpp.cpp)
add_executable(green_thread_cpp_timer green_thread_cpp_timer.cpp)
add_executable(green_thread_cpp_fd green_thread_cpp_fd.cpp)
add_executable(green_thread_cpp_stream green_thread_cpp_stream.cpp)
add_executable(green_thread_cpp_threadq green_thread_cpp_threadq.cpp)
add_executable(green_thread_cpp_all green_thread_cpp_all.cpp)

if(CMAKE_THREAD_LIBS_INIT)
    set(LIBS ${LLVM_AVAILABLE_LIBS}
             ${CMAKE_CURRENT_LIST_DIR}/../../src/liblunarlang_static.a
             ${CMAKE_THREAD_LIBS_INIT})
else()
    set(LIBS ${LLVM_AVAILABLE_LIBS} ${CMAKE_CURRENT_LIST_DIR}/../../src/liblunarlang_static.a)
endif()

target_link_libraries(green_thread_cpp ${LIBS})
target_link_libraries(green_thread_cpp_timer ${LIBS})
target_link_libraries(green_thread_cpp_fd ${LIBS})
target_link_libraries(green_thread_cpp_stream ${LIBS})
target_link_libraries(green_thread_cpp_threadq ${LIBS})
target_link_libraries(green_thread_cpp_all ${LIBS})
