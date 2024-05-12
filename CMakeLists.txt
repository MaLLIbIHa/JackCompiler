cmake_minimum_required(VERSION 3.10)
project(JackCompiler)

find_package(LLVM REQUIRED CONFIG PATHS D:/clang/llvm-project/build/lib/cmake/llvm)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set your project compile flags.
# E.g. if using the C++ header files
# you will need to enable C++11 support
# for your compiler.

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Now build our tools
set(SRC_LIST compiler/parser.cpp
             driver.cpp)

add_executable(${PROJECT_NAME} ${SRC_LIST})

target_compile_features(${PROJECT_NAME} PRIVATE cxx_std_17)

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader)

# Link against LLVM libraries
target_link_libraries(${PROJECT_NAME} ${llvm_libs})