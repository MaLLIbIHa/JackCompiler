#pragma once
#include "NodeDescriptors.hpp"
#include "Type.hpp"
#include "compiler/AST2.hpp"
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

enum class SymbolType {
  CLASS_SYM,
  FIELD_VARIABLE_SYM,
  STATIC_VARIABLE_SYM,
  LOCAL_VARIABLE_SYM,
  ARGUMENT_VARIABLE_SYM,
  SUBROUTINE_SYM,
};

class SymbolNode {
public:
  SymbolNode() = default;
  SymbolNode(std::string name, SourceLocation srcLoc)
      : name_(std::move(name)), srcLoc_(srcLoc) {}

  virtual SymbolType getSymbolType() const = 0;
  virtual ~SymbolNode() = default;

  void setName(std::string name) { name_ = std::move(name); }
  void setSourceLoc(SourceLocation srcLoc) { srcLoc_ = srcLoc; }

  std::string getName() const { return name_; }
  SourceLocation getSourceLocation() const { return srcLoc_; }
  unsigned int getInLinePos() const { return srcLoc_.getInLinePos(); }
  unsigned int getLinePos() const { return srcLoc_.getLinePos(); }

protected:
  std::string name_;
  SourceLocation srcLoc_;
};

class SymbolTable final {
public:
  std::shared_ptr<SymbolNode> find(std::string name) const {
    auto symbol = table_.find(name);
    if (symbol != table_.end()) {
      return symbol->second;
    }
    return nullptr;
  }

  bool insert(std::string name, std::shared_ptr<SymbolNode> symbol) {
    if (table_.find(name) != table_.end()) {
      return false;
    }
    table_[name] = symbol;
    return true;
  }

  void clear() { table_.clear(); }

private:
  std::unordered_map<std::string, std::shared_ptr<SymbolNode>> table_;
};

class ClassSymbol final : public SymbolNode {
public:
  ClassSymbol() = default;
  ClassSymbol(std::string name, ClassType *classType, SourceLocation srcLoc)
    : SymbolNode(std::move(name), srcLoc), classType_(classType) {}

  void setClassType(ClassType *classType) { classType_ = classType; }

  SymbolType getSymbolType() const override { return SymbolType::CLASS_SYM; }
  ClassType *getClassType() { return classType_; }

  std::shared_ptr<SymbolNode> findMember(std::string name) const {
    return symTable_.find(name);
  }

  void addSymbol(std::string name, std::shared_ptr<SymbolNode> symbol) {
    symTable_.insert(name, symbol);
  }

  void clear() {
    symTable_.clear();
    name_.clear();
  }

private:
  SymbolTable symTable_;
  ClassType *classType_;
};

class SubroutineSymbol final : public SymbolNode {
public:
  SubroutineSymbol() = default;

  SubroutineSymbol(std::string name, const Type *retT, 
                   std::vector<const Type *> args, SubroutineKind sType,
                   SourceLocation srcLoc, bool haveBody)
      : SymbolNode(name, srcLoc), retType_(retT),
        argsTypes_(std::move(args)), sType_(sType), haveBody_(haveBody) {}

  void setKind(SubroutineKind kind) { sType_ = kind; }
  void setRetType(Type *retType) { retType_ = retType; }
  void addArgType(Type *type) { argsTypes_.push_back(type); }

  SubroutineKind getKind() const { return sType_; }
  const Type *getRetType() const { return retType_; }
  unsigned getArgsCount() const { return argsTypes_.size(); }
  const Type *getArgType(unsigned i) const { return argsTypes_[i]; }
  bool haveBody() const { return haveBody_; }

  std::vector<const Type *>::const_iterator arg_type_begin() const {
    return argsTypes_.cbegin();
  }
  std::vector<const Type *>::const_iterator arg_type_end() const {
    return argsTypes_.cend();
  }

  SymbolType getSymbolType() const override {
    return SymbolType::SUBROUTINE_SYM;
  }

private:
  std::vector<const Type *> argsTypes_;
  const Type *retType_;
  SubroutineKind sType_;
  bool haveBody_;
};

class VarSymbol : public SymbolNode {
public:
  VarSymbol(std::string name, const Type *vType, SourceLocation srcLoc)
      : SymbolNode(name, srcLoc), vType_(vType) {}

  virtual SymbolType getSymbolType() const = 0;
  
  const Type *getVarType() const { return vType_; }
  void setType(Type *type) { vType_ = type; }

private:
  const Type *vType_;
};

class LocalVarSymbol final : public VarSymbol {
public:
  using VarSymbol::VarSymbol;
  
  SymbolType getSymbolType() const override {
    return SymbolType::LOCAL_VARIABLE_SYM;
  }
};

class ArgumentVarSymbol final : public VarSymbol {
public:
  using VarSymbol::VarSymbol;

  SymbolType getSymbolType() const override {
    return SymbolType::ARGUMENT_VARIABLE_SYM;
  }
};

class FieldVarSymbol final : public VarSymbol {
public:
  using VarSymbol::VarSymbol;

  FieldVarSymbol(std::string name, const Type *vType,
                      ClassSymbol *parentClass, unsigned index,
                      SourceLocation srcLoc)
      : VarSymbol(name, vType, srcLoc), parentClass_(parentClass),
        index_(index) {}

  ClassSymbol *getParentClass() const { return parentClass_; }
  unsigned getIndex() const { return index_; }

  SymbolType getSymbolType() const override {
    return SymbolType::FIELD_VARIABLE_SYM;
  }
private:
  ClassSymbol *parentClass_;
  unsigned index_;
};

class StaticVarSymbol final : public VarSymbol {
public:
  using VarSymbol::VarSymbol;

  StaticVarSymbol(std::string name, const Type *vType, ClassSymbol *parentClass,
                       SourceLocation srcLoc)
      : VarSymbol(name, vType, srcLoc), parentClass_(parentClass) {}

  ClassSymbol *getParentClass() const { return parentClass_; }
  SymbolType getSymbolType() const override { 
    return SymbolType::STATIC_VARIABLE_SYM; 
  }
private:
  ClassSymbol *parentClass_;
};
