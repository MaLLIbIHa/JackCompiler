#include <sstream>
#include <string>

class SourceLocation final {
public:
  SourceLocation(std::string fileName, 
                 unsigned linePos,
                 unsigned inLinePos)
      : sourceFileName_(fileName),
        linePos_(linePos),
        inLinePos_(inLinePos) {}

  std::string getFileName() const { return sourceFileName_; }

  unsigned getLinePos() const { return linePos_; }

  unsigned getInLinePos() const { return inLinePos_; }

  std::string toStr() const {
    std::stringstream sstream;
    sstream << linePos_ << ':' << inLinePos_;
    return sstream.str();
  }

private:
  std::string sourceFileName_;
  unsigned linePos_;
  unsigned inLinePos_;
};