#include <string>
#include <vector>
#include <filesystem>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iterator>
#include "compiler\Parser.hpp"
#include "compiler\PrintVisitor.hpp"
#include "compiler\CodegenVisitor.hpp"

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

    if (argc < 2) {
        std::cerr << "No input file or directory" << std::endl;
        return 1;
    }

    std::filesystem::path path = argv[1];
    std::error_code ec;
    if (std::filesystem::exists(path, ec)) {
        if (checkErr(ec)) return 1;

        if (std::filesystem::is_directory(path, ec)) {
            if (checkErr(ec)) return 1;

            for (const auto &entry : std::filesystem::directory_iterator(path)) {
                if (entry.path().extension() == ".jack") {
                    std::ifstream ifstr(entry.path().string());
                    if (ifstr.fail()) {
                        std::cerr << "Error occurred while opening file" << std::endl;
                        return 1;
                    }
                    filenames.push_back(entry.path().filename().string());
                    fileStreams.push_back(std::move(ifstr));
                }
            }

        } else {
            if (path.extension() == ".jack") {
                std::ifstream ifstr(path.string());
                if (ifstr.fail()) {
                    std::cerr << "Error occurred while opening file" << std::endl;
                    return 1;
                }
                filenames.push_back(path.filename().string());
                fileStreams.push_back(std::move(ifstr));
            }
        }

    } else {
        std::cerr << "Cannot find " << path << ": No such file or directory" << std::endl;
        return 1;
    }

    if (fileStreams.size() == 0) {
        std::cerr << "No files were provided" << std::endl;
        return 1;
    }

    // compiler::lexer lex(inputTexts[0]);
    // std::vector<compiler::token> toks = lex.lex();
    // compiler::parser p(toks);
    // std::sharedPtr<compiler::program> prog = p.parseProgram();
    // compiler::PrintVisitor classVis;
    // classVis.traversal(prog);

    std::string sourceText(std::istreambuf_iterator<char>(fileStreams[0]),
                           std::istreambuf_iterator<char>{});
    try {
        Parser p(filenames[0], sourceText);
        PrintVisitor printVstr;
        CodegenVisitor semaVstr;
        auto fileUnit = p.parseFileUnit();
        Program prog;
        prog.addFile(fileUnit);
        prog.accept(semaVstr);
    } catch (std::runtime_error& err) {
        std::cerr << err.what() << std::endl;
        return 1;
    }

    return 0;
}