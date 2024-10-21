#pragma once
#include "Type.hpp"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <vector>

class IRBuilderWrapper final {
public:
  IRBuilderWrapper()
      : context_(std::make_unique<llvm::LLVMContext>()),
        module_(std::make_unique<llvm::Module>("JackProgram", *context_)),
        builder_(std::make_unique<llvm::IRBuilder<>>(*context_)) {}

  llvm::Function *createFunction(const std::string &name, Type *retType,
                                 const std::vector<std::string> &argNames,
                                 const std::vector<Type *> &argTypes) {
    std::vector<llvm::Type *> llvmArgTypes(argTypes.size());
    unsigned idx = 0;
    for (Type *type : argTypes) {
      llvmArgTypes[idx++] = convertTypeToLLVMType(type);
    }

    llvm::FunctionType *funcType = llvm::FunctionType::get(
        convertTypeToLLVMType(retType), llvmArgTypes, false);
    llvm::Function *func = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, name, module_.get());

    idx = 0;
    for (auto &&arg : func->args()) {
      arg.setName(argNames[idx++]);
    }
  }

private:
  llvm::Type *convertTypeToLLVMType(Type *type) {
    switch (type->getTypeId()) {
    case Type::TypeId::IntTy:
      return llvm::Type::getInt32Ty(*context_);
    case Type::TypeId::CharTy:
      return llvm::Type::getInt8Ty(*context_);
    case Type::TypeId::BoolTy:
      return llvm::Type::getInt1Ty(*context_);
    case Type::TypeId::VoidTy:
      return llvm::Type::getVoidTy(*context_);
    case Type::TypeId::NullTy:
      return llvm::PointerType::get(*context_, 0);
    case Type::TypeId::ClassTy:
      return llvm::PointerType::get(*context_, 0);
    case Type::TypeId::ArrayTy:
      return llvm::PointerType::get(*context_, 0);
    }

    return nullptr;
  }

  llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *function,
                                           const std::string &varName,
                                           llvm::Type *varType) {
    llvm::IRBuilder<> TmpB(&function->getEntryBlock(),
                           function->getEntryBlock().begin());
    return TmpB.CreateAlloca(varType, nullptr, varName);
  }

private:
  std::unordered_map<std::string, llvm::AllocaInst *> namedValues_;
  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::IRBuilder<>> builder_;
  std::unique_ptr<llvm::Module> module_;
};