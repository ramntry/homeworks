#pragma once
#include <string>
#include "grammar.h"

class NetconfParser
{
public:
    NetconfParser(Constructor *constructor);
    bool parse_file(std::string const &filename);
    bool parse_string(std::string const &input);

private:
    typedef std::string::const_iterator input_iterator;

    netconf_grammar<input_iterator> grammar_;
};

