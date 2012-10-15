#pragma once
#include <vector>
#include <string>
#include "pc.h"

class Constructor
{
public:
    typedef std::vector<pc> pc_group;

    void chain(pc_group const &group);
    void star(pc_group const &group);
    void cicle(pc_group const &group);
    void each_to_each(pc_group const &group);

private:
    void print_expression(std::string const &operator_name, pc_group const &group);
};


