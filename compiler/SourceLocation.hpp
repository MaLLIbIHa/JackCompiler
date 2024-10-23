#include <sstream>
#include <string>

class SourceLocation final {
public:
  SourceLocation() = default;
  SourceLocation(unsigned linePos, unsigned inLinePos)
      : linePos_(linePos), inLinePos_(inLinePos) {}

  unsigned getLinePos() const { return linePos_; }
  unsigned getInLinePos() const { return inLinePos_; }

  std::string toStr() const {
    std::stringstream sstream;
    sstream << linePos_ << ':' << inLinePos_;
    return sstream.str();
  }

private:
  unsigned linePos_ = 0;
  unsigned inLinePos_ = 0;
};