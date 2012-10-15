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

inline bool operator <(pc const &lhs, pc const &rhs)
{
    return lhs.name < rhs.name;
}
