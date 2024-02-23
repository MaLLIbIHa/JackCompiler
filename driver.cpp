#include <string>
#include <vector>
#include <filesystem>
#include <iostream>
#include <fstream>
#include <sstream>
#include "vm.hpp"

bool check_err(std::error_code &ec) {
    bool err = static_cast<bool>(ec);
    if (err) {
        std::cerr << ec << std::endl;
    }
    return err;
}

bool read_vm_file(std::filesystem::path path, 
                  std::vector<std::string> &buffer, 
                  std::vector<std::string> &filenames) {
    if (path.extension() != ".vm") return false;

    std::ifstream source_file(path.string());
    if (source_file.is_open()) {
        std::ostringstream contents;
        contents << source_file.rdbuf();
        buffer.push_back(contents.str());
        filenames.push_back(path.filename().string());
    } else {
        if (std::filesystem::is_directory(path)) return false;

        std::cerr << "Can't open file " << path.filename() << std::endl;
        return true;
    }
    return false;
}

int main(int argc, char** argv) {
    std::vector<std::string> input_texts;
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
                if (read_vm_file(entry.path(), input_texts, filenames)) return 1;
            }

        } else {
            if (read_vm_file(path, input_texts, filenames)) return 1;
        }

    } else {
        std::cerr << "Cannot find " << path << ": No such file or directory" << std::endl;
        return 1;
    }

    if (input_texts.size() == 0) {
        std::cerr << "No files were provided" << std::endl;
        return 1;
    }

    virtual_machine vm(input_texts[0]);
    if (vm.lex()) return 1;
    vm.parse();
    vm.print();

    return 0;
}