#include <iostream>
#include <algorithm>
#include <boost/foreach.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/io.hpp>
#include "Constructor.h"

Constructor::Constructor(VirtualServer *server, Network *network)
    : server_(server)
    , network_(network)
{
}

void Constructor::print_expression(std::string const &operator_name
    , Constructor::pc_group const &arguments)
{
    std::cout << operator_name << ':';
    BOOST_FOREACH(pc const &pc_object, arguments) {
        std::cout << ' ' << boost::fusion::as_vector(pc_object);
    }
    std::cout << std::endl;
}

std::vector<NetworkAddress> Constructor::check_existence(pc_group const &group)
{
    std::vector<NetworkAddress> addresses;
    BOOST_FOREACH(pc const &pc_object, group) {
        pc_table::iterator finded = pc_table_.find(pc_object);
        if (finded == pc_table_.end()) {
            NetworkAddress address = network_->addDevice(
                server_->addPC(pc_object.name, pc_object.os));
            pc_table_[pc_object] = address;
            addresses.push_back(address);
        } else {
            if (pc_object.os != finded->first.os) {
                std::cerr << "parse failed: os type error for pc \""
                          << pc_object.name << '"' << std::endl;
                exit(4);
            }
            addresses.push_back(finded->second);
        }
    }
    return addresses;
}

void Constructor::chain(Constructor::pc_group const &group)
{
    std::vector<NetworkAddress> addresses(check_existence(group));
    for (size_t i = 1; i < addresses.size(); ++i)
    {
        network_->link(addresses[i - 1], addresses[i]);
    }
}

void Constructor::star(Constructor::pc_group const &group)
{
    std::vector<NetworkAddress> addresses(check_existence(group));
    for (size_t i = 1; i < addresses.size(); ++i)
    {
        network_->link(addresses[0], addresses[i]);
    }
}

void Constructor::cicle(Constructor::pc_group const &group)
{
    std::vector<NetworkAddress> addresses(check_existence(group));
    for (size_t i = 0; i < addresses.size(); ++i)
    {
        network_->link(addresses[i], addresses[(i + 1) % addresses.size()]);
    }
}

void Constructor::each_to_each(Constructor::pc_group const &group)
{
    std::vector<NetworkAddress> addresses(check_existence(group));
    for (size_t i = 0; i < addresses.size() - 1; ++i)
    {
        for (size_t j = i + 1; j < addresses.size(); ++j)
        {
            network_->link(addresses[i], addresses[j]);
        }
    }
}

