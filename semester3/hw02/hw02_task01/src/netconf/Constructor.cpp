#include <iostream>
#include <boost/foreach.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/io.hpp>
#include "Constructor.h"

void Constructor::print_expression(std::string const &operator_name, Constructor::pc_group const &arguments)
{
    std::cout << operator_name << ':';
    BOOST_FOREACH(pc const &pc_object, arguments) {
        std::cout << ' ' << boost::fusion::as_vector(pc_object);
    }
    std::cout << std::endl;
}

void Constructor::chain(Constructor::pc_group const &group)
{
    print_expression("chain", group);
}

void Constructor::star(Constructor::pc_group const &group)
{
    print_expression("star", group);
}

void Constructor::cicle(Constructor::pc_group const &group)
{
    print_expression("cicle", group);
}

void Constructor::each_to_each(Constructor::pc_group const &group)
{
    print_expression("each_to_each", group);
}

