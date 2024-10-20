#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

class Type {
  using Context = llvm::LLVMContext;

  enum class TypeId {
    IntTy,
    CharTy,
    BoolTy,
    ClassTy,
    ArrayTy,
    NullTy
  };

private:
  Type(llvm::Type* llvmType, TypeId typeId) 
      : llvmType_(llvmType), typeId_(typeId) {}
public:
  static Type getIntType(Context &context) {
    return {llvm::Type::getInt32Ty(context), TypeId::IntTy};
  }

  static Type getCharType(Context &context) {
    return {llvm::Type::getInt8Ty(context), TypeId::CharTy};
  }

  static Type getBoolType(Context &context) {
    return {llvm::Type::getInt1Ty(context), TypeId::BoolTy};
  }

  static Type getNullType(Context &context) {
    auto voidTy = llvm::Type::getVoidTy(context);
    return {llvm::PointerType::get(voidTy, 0), TypeId::NullTy};
  }

  static Type getArrayType(Type type) {
    return {llvm::PointerType::get(type.llvmType_, 0), TypeId::ArrayTy};
  }

  static Type getClassType(const std::vector<Type>& fieldTypes,
                           const std::string& name) {
    std::vector<llvm::Type*> llvmTypes;
    llvmTypes.reserve(fieldTypes.size());
    for (Type type : fieldTypes) {
      llvmTypes.push_back(type.llvmType_);
    }
    auto llvmStructTy = llvm::StructType::create(llvmTypes, name);
    return {llvm::PointerType::get(llvmStructTy, 0), TypeId::ClassTy};
  }

  bool isIntTy() const { return typeId_ == TypeId::IntTy; }

  bool isCharTy() const { return typeId_ == TypeId::CharTy; }

  bool isBoolTy() const { return typeId_ == TypeId::BoolTy; }

  bool isArrayTy() const { return typeId_ == TypeId::ArrayTy; }

  bool isClassTy() const { return typeId_ == TypeId::ClassTy; }

  bool isNullTy() const { return typeId_ == TypeId::NullTy; }

  bool operator==(const Type &other) const {
    if (typeId_ == TypeId::NullTy && other.typeId_ == TypeId::ArrayTy ||
        typeId_ == TypeId::NullTy && other.typeId_ == TypeId::ClassTy ||
        typeId_ == TypeId::ArrayTy && other.typeId_ == TypeId::NullTy ||
        typeId_ == TypeId::ClassTy && other.typeId_ == TypeId::NullTy ||
        llvmType_ == other.llvmType_ && typeId_ == other.typeId_) {
      return true;
    }

    return false;
  }

  bool operator!=(const Type &other) const {
    return !(*this == other);
  }

private:
  llvm::Type* llvmType_;
  TypeId typeId_;
};