set(TESTS exmpl.cpp)
add_executable(Test ${TESTS})
target_link_libraries(Test ${llvm_libs} GTest::gtest_main)