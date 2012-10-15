#include <iostream>
#include <custom/support/read_from_file.hpp>
#include "NetconfParser.h"

NetconfParser::NetconfParser(Constructor *constructor)
    : grammar_(constructor)
{
}

bool NetconfParser::parse_file(std::string const &filename)
{
    return parse_string(read_from_file(filename));
}

bool NetconfParser::parse_string(std::string const &input)
{
    input_iterator it = input.begin();
    input_iterator end = input.end();

    bool ok = qi::phrase_parse(it, end, grammar_, ascii::space) && it == end;

    if (!ok) {
        std::cerr << "parsing failed at:\n\t"
                  << std::string(it, std::find(it, end, '\n')) << std::endl;
    }
    return ok;
}

