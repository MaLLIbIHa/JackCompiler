#pragma once
#include "NodeDescriptors.hpp"
#include "Type.hpp"
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
  SymbolNode(std::string name, unsigned linePos, unsigned inLinePos)
      : name_(std::move(name)), linePos_(linePos), inLinePos_(inLinePos) {}

  virtual SymbolType getSymbolType() const = 0;

  virtual ~SymbolNode() = default;

  void setName(std::string name) { name_ = std::move(name); }

  void setLinePos(unsigned int linePos) { linePos_ = linePos; }

  void setInLinePos(unsigned int inLinePos) { inLinePos_ = inLinePos; }

  std::string getName() const { return name_; }

  unsigned int getInLinePos() const { return inLinePos_; }

  unsigned int getLinePos() const { return linePos_; }

protected:
  std::string name_;
  unsigned linePos_ = 0;
  unsigned inLinePos_ = 0;
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
  ClassSymbol(std::string name, Type *classType, unsigned linePos,
              unsigned inLinePos)
      : SymbolNode(name, linePos, inLinePos), classType_(classType) {}

  SymbolType getSymbolType() const override { return SymbolType::CLASS_SYM; }

  void setClassType(Type *classType) { classType_ = classType; }

  Type *getClassType() { return classType_; }

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
  Type *classType_;
};

class SubroutineSymbol final : public SymbolNode {
public:
  SubroutineSymbol() = default;

  SubroutineSymbol(std::string name, Type *retT, std::vector<Type *> args,
                   SubroutineKind sType, unsigned linePos, unsigned inLinePos)
      : SymbolNode(name, linePos, inLinePos), retType_(retT),
        argsTypes_(std::move(args)), sType_(sType) {}

  SymbolType getSymbolType() const override {
    return SymbolType::SUBROUTINE_SYM;
  }

  void setKind(SubroutineKind kind) { sType_ = kind; }

  SubroutineKind getKind() const { return sType_; }

  void setRetType(Type *retType) { retType_ = retType; }

  Type *getRetType() const { return retType_; }

  void addArgType(Type *type) { argsTypes_.push_back(type); }

  Type *getArgType(unsigned i) const { return argsTypes_[i]; }

  unsigned getArgCount() const { return argsTypes_.size(); }

private:
  std::vector<Type *> argsTypes_;
  Type *retType_;
  SubroutineKind sType_;
};

class VarSymbol : public SymbolNode {
public:
  VarSymbol(std::string name, Type *vType, unsigned linePos, unsigned inLinePos)
      : SymbolNode(name, linePos, inLinePos), vType_(vType) {}

  virtual SymbolType getSymbolType() const = 0;

  void setType(Type *type) { vType_ = type; }

  Type *getVarType() const { return vType_; }

private:
  Type *vType_;
};

class LocalVariableSymbol final : public VarSymbol {
public:
  SymbolType getSymbolType() const override {
    return SymbolType::LOCAL_VARIABLE_SYM;
  }
};

class ArgumentVariableSymbol final : public VarSymbol {
public:
  SymbolType getSymbolType() const override {
    return SymbolType::ARGUMENT_VARIABLE_SYM;
  }
};

class FieldVariableSymbol final : public VarSymbol {
public:
  FieldVariableSymbol(std::string name, Type *vType, ClassSymbol *parentClass,
                      unsigned index, unsigned linePos, unsigned inLinePos)
      : VarSymbol(name, vType, linePos, inLinePos), parentClass_(parentClass),
        index_(index) {}

  SymbolType getSymbolType() const override {
    return SymbolType::FIELD_VARIABLE_SYM;
  }

  ClassSymbol *getParentClass() const { return parentClass_; }

  unsigned getIndex() const { return index_; }

private:
  ClassSymbol *parentClass_;
  unsigned index_;
};

class StaticVariableSymbol final : public VarSymbol {
public:
  StaticVariableSymbol(std::string name, Type *vType, ClassSymbol *parentClass,
                       unsigned linePos, unsigned inLinePos)
      : VarSymbol(name, vType, linePos, inLinePos), parentClass_(parentClass) {}

  SymbolType getSymbolType() const override {
    return SymbolType::STATIC_VARIABLE_SYM;
  }

  ClassSymbol *getParentClass() const { return parentClass_; }

private:
  ClassSymbol *parentClass_;
};
