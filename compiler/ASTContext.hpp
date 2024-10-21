#pragma once
#include <concepts>
#include <utility>
#include <vector>
#include <memory>
#include "AST2.hpp"

class ASTContext {
public:
  ASTContext(std::string programName) : program_(std::move(programName)) {}

  template<typename NodeType, typename... Args>
  requires std::derived_from<NodeType, Node>
  NodeType* createNode(Args&&... constructorArgs) {
    auto node = std::make_unique<NodeType>(std::forward<Args>(constructorArgs)...);
    ASTNodes_.push_back(std::move(node));
    if constexpr (std::same_as<NodeType, ClassDec>) 
      program_.addClassDec(node);
    return node.get();
  }

  void accept(Visitor&& v) const {
    program_.accept(v);
  }

private:
  Program program_;
  std::vector<std::unique_ptr<Node>> ASTNodes_;
};