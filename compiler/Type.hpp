#pragma once
#include <unordered_map>
#include <memory>
#include <string>

class Type {

public:
  enum class TypeId {
    IntTy,
    CharTy,
    BoolTy,
    ClassTy,
    ArrayTy,
    NullTy,
    VoidTy
  };

protected:
  Type(TypeId typeId) : typeId_(typeId) {}

public:
  static bool isAssignable(Type* lhs, Type* rhs) {
    if ((lhs->typeId_ == TypeId::NullTy && rhs->typeId_ == TypeId::ArrayTy) ||
        (lhs->typeId_ == TypeId::NullTy && rhs->typeId_ == TypeId::ClassTy) ||
        (lhs->typeId_ == TypeId::ArrayTy && rhs->typeId_ == TypeId::NullTy) ||
        (lhs->typeId_ == TypeId::ClassTy && rhs->typeId_ == TypeId::NullTy) ||
        lhs == rhs) {
      return true;
    }

    return false;
  }

  static Type* getIntTy() {
    static Type t(TypeId::IntTy);
    return &t;
  }

  static Type* getCharTy() {
    static Type t(TypeId::CharTy);
    return &t;
  }

  static Type* getBoolTy() {
    static Type t(TypeId::BoolTy);
    return &t;
  }

  static Type* getNullTy() {
    static Type t(TypeId::NullTy);
    return &t;
  }

  static Type* getVoidTy() {
    static Type t(TypeId::VoidTy);
    return &t;
  }

  TypeId getTypeId() const { return typeId_; }

  // if Type is ArrayType
  Type* getArrayElementType() const;

  // if Type is ClassType
  std::string getClassName() const;

  std::string toString() const;

  bool isIntTy() const { return typeId_ == TypeId::IntTy; }

  bool isCharTy() const { return typeId_ == TypeId::CharTy; }

  bool isBoolTy() const { return typeId_ == TypeId::BoolTy; }

  bool isArrayTy() const { return typeId_ == TypeId::ArrayTy; }

  bool isClassTy() const { return typeId_ == TypeId::ClassTy; }

  bool isNullTy() const { return typeId_ == TypeId::NullTy; }

  bool isVoidTy() const { return typeId_ == TypeId::VoidTy; }

private:
  TypeId typeId_;
};

class ArrayType final : public Type {

  inline static std::unordered_map<Type*, ArrayType> arrayTypeContainer_;

public:
  static ArrayType* getArrayTy(Type* type) {
    auto type_it = arrayTypeContainer_.find(type);
    if (type_it == arrayTypeContainer_.end()) {
      ArrayType t(TypeId::ArrayTy, type);
      auto [elementIt, res] = arrayTypeContainer_.insert(
          {type, std::move(t)});
      return &elementIt->second;
    } else {
      return &type_it->second;
    }
  }
private:
  ArrayType(TypeId typeId, Type* containedType) 
      : Type(typeId), containedType_(containedType) {}

public:
  Type* getArrayElementType() const { return containedType_; }

private:
  Type* containedType_;
};

inline Type* Type::getArrayElementType() const {
  auto arrType = static_cast<ArrayType const*>(this);
  return arrType->getArrayElementType();
}

class ClassType final : public Type {

  inline static std::unordered_map<std::string, ClassType> classTypeContainer_;

public:
  static ClassType* getClassTy(const std::string& name) {
    auto type_it = classTypeContainer_.find(name);
    if (type_it == classTypeContainer_.end()) {
      ClassType t(TypeId::ClassTy, name);
      auto [elementIt, res] = classTypeContainer_.insert(
          {name, std::move(t)});
      return &elementIt->second;
    } else {
      return &type_it->second;
    }
  }
private:
  ClassType(TypeId typeId, std::string className)
      : Type(typeId), className_(std::move(className)) {}
public:
  std::string getClassName() const { return className_; }
private:
  std::string className_;
};

inline std::string Type::getClassName() const {
  auto classType = static_cast<ClassType const*>(this);
  return classType->getClassName();
}


inline std::string Type::toString() const {
  Type const* type = this;
  std::string suffix;
  if (isArrayTy()) {
    type = getArrayElementType();
    suffix = "[]";
  }

  std::string typeStr = [](Type const* type) -> std::string {
  	switch(type->getTypeId()) {
  	case TypeId::BoolTy:
  		return "bool";
  	case TypeId::IntTy:
  		return "int";
  	case TypeId::CharTy:
  		return "char";
  	case TypeId::NullTy:
  		return "null";
  	case TypeId::VoidTy:
  		return "void";
  	case TypeId::ClassTy:
  		return static_cast<ClassType const*>(type)->getClassName();
  	}
  	return "";
  }(type);

  return typeStr + suffix;
}