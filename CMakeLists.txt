cmake_minimum_required(VERSION 3.10)

project(JackCompiler)

set(SRC_LIST compiler/lexer.cpp 
             compiler/parser.cpp
             compiler/token.cpp
             vm/lexer.cpp
             vm/parser.cpp
             vm/token.cpp
             driver.cpp)

add_executable(${PROJECT_NAME} ${SRC_LIST})