#include "gtest/gtest.h"
#include "llvm/IR/LLVMContext.h"

TEST(lexer_test_suit, lexer_test) {
  std::stringstream s("class");
  llvm::LLVMContext ctx;
  EXPECT_EQ(&ctx, &ctx);
}