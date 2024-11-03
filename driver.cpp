#include <string>
#include <vector>
#include <filesystem>
#include <iostream>
#include <fstream>
#include "compiler/ASTContext.hpp"
#include "compiler/Parser.hpp"
#include "compiler/PrintVisitor.hpp"
#include "compiler/CodegenVisitor.hpp"

bool checkErr(std::error_code &ec) {
  bool err = static_cast<bool>(ec);
  if (err) {
    std::cerr << ec << std::endl;
  }
  return err;
}

int main(int argc, char** argv) {
  std::vector<std::ifstream> fileStreams;
  std::vector<std::string> filenames;

  if (argc < 3) {
    std::cerr << "Usage: JackCompiler <file> <stdlib-path>" << std::endl;
    return 1;
  }

  std::error_code ec;
  for (unsigned idx = 1; idx < 3; ++idx) {
    std::filesystem::path path = argv[idx];
    if (std::filesystem::exists(path, ec)) {
      if (checkErr(ec)) return 1;
      if (std::filesystem::is_regular_file(path) && path.extension() == ".jack") {
        std::ifstream ifstr(path.string());
        if (ifstr.fail()) {
          std::cerr << "Error occurred while opening file" << std::endl;
          return 1;
        }
        filenames.push_back(path.filename().string());
        fileStreams.push_back(std::move(ifstr));
      }
    } else {
      std::cerr << "Cannot find " << path << ": No such file or directory" << std::endl;
      return 1;
    }
  }

  if (fileStreams.size() == 0) {
    std::cerr << "No files were provided" << std::endl;
    return 1;
  }

  try {
    ASTContext ctx(filenames[0]);
    Parser stdLibPr(fileStreams[1], ctx);
    stdLibPr.parseFile();
    Parser programPr(fileStreams[0], ctx);
    programPr.parseFile();
    CodegenVisitor codegenVstr;
    ctx.accept(codegenVstr);
  } catch (std::runtime_error& err) {
    std::cerr << err.what() << std::endl;
    return 1;
  }

    return 0;
}