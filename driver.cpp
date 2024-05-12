#include <string>
#include <vector>
#include <filesystem>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iterator>
#include "compiler\parser.hpp"
#include "compiler\print_visitor.hpp"
#include "compiler\semantic_visitor.hpp"
#include "compiler\codegen_visitor.hpp"

bool check_err(std::error_code &ec) {
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
        if (check_err(ec)) return 1;

        if (std::filesystem::is_directory(path, ec)) {
            if (check_err(ec)) return 1;

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

    // compiler::lexer lex(input_texts[0]);
    // std::vector<compiler::token> toks = lex.lex();
    // compiler::parser p(toks);
    // std::shared_ptr<compiler::program> prog = p.parse_program();
    // compiler::PrintVisitor classVis;
    // classVis.traversal(prog);

    std::string sourceText(std::istreambuf_iterator<char>(fileStreams[0]),
                           std::istreambuf_iterator<char>{});
    try {
        compiler::parser p(sourceText);
        compiler::print_visitor print_vstr;
        compiler::semantic_visitor sema_vstr;
        auto prog = p.parse_program();
        prog->accept(sema_vstr);
    } catch (std::runtime_error& err) {
        std::cerr << err.what() << std::endl;
        return 1;
    }

    return 0;
}