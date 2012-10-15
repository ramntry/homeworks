#pragma once
#include <vector>
#include <string>
#include <map>
#include "../VirtualServer.h"
#include "../Network.h"
#include "pc.h"

class Constructor
{
public:
    typedef std::vector<pc> pc_group;

    Constructor(VirtualServer *server, Network *network);

    void chain(pc_group const &group);
    void star(pc_group const &group);
    void cicle(pc_group const &group);
    void each_to_each(pc_group const &group);

private:
    typedef std::map<pc, NetworkAddress> pc_table;

    void print_expression(std::string const &operator_name, pc_group const &group);
    std::vector<NetworkAddress> check_existence(pc_group const &group);

    VirtualServer *server_;
    Network *network_;
    pc_table pc_table_;
};


