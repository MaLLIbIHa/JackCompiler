#include <vector>
#include <concepts>
#include "AST.hpp"

class ASTContainer {
public:
  template <typename T, typename ...Args>
    requires std::derived_from<T, Node>
    T* create(Args&&... args) {
        nodes_.push_back(std::make_unique<T>(std::forward<Args>(args)...));
        return static_cast<T*>(nodes_.back().get());
    }

private:
  std::vector<std::unique_ptr<Node>> nodeContainer_;
};