#pragma once
#include <string>
#include <boost/fusion/include/adapt_struct.hpp>
#include "../OS.h"

struct pc
{
    OS os;
    std::string name;
};

BOOST_FUSION_ADAPT_STRUCT(pc,
    (OS, os)
    (std::string, name)
);

