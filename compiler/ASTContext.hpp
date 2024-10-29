#pragma once
#include "AST2.hpp"
#include <concepts>
#include <memory>
#include <utility>
#include <vector>

class ASTContext {
public:
  ASTContext(std::string programName) : program_(std::move(programName)) {}

  template <typename NodeType, typename... Args>
  requires std::derived_from<NodeType, Node> NodeType *
  createNode(Args &&...constructorArgs) {
    auto node =
        std::make_unique<NodeType>(std::forward<Args>(constructorArgs)...);
    auto rawNodePtr = node.get();
    ASTNodes_.push_back(std::move(node));
    if constexpr (std::same_as<NodeType, ClassDec>)
      program_.addClassDec(rawNodePtr);
    return rawNodePtr;
  }

  void accept(Visitor &v) const { program_.accept(v); }

private:
  Program program_;
  std::vector<std::unique_ptr<Node>> ASTNodes_;
};