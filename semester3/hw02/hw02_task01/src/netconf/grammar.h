#pragma once
#include <vector>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/bind.hpp>

#include "Constructor.h"

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

struct os_table : qi::symbols<char, OS>
{
    os_table()
    {
        add
            ("win", Windows)
            ("mac", MacOS)
            ("lin", Linux)
        ;
    }
};

template <typename I>
struct netconf_grammar : qi::grammar<I, ascii::space_type>
{
    netconf_grammar(Constructor *constructor)
        : netconf_grammar::base_type(net, "netconf-grammar")
        , constructor_(constructor)
    {
        pc_name   %= qi::lexeme[qi::alpha >> *(qi::alnum | qi::char_('_'))];
        pc_object %= pc_os >> -qi::lit('_') >> pc_name;
        pc_group  %= '(' >> qi::repeat(3, qi::inf)[pc_object] >> ')';

        chain        %= pc_object >> +('-' >> pc_object);
        star         %= pc_object >> (qi::lit('*') | "star")         >> pc_group;
        cicle        %=              (qi::lit('@') | "cicle")        >> pc_group;
        each_to_each %=              (qi::lit('#') | "each_to_each") >> pc_group;

        expression = chain        [boost::bind(&Constructor::chain,        constructor_, _1)]
                   | star         [boost::bind(&Constructor::star,         constructor_, _1)]
                   | cicle        [boost::bind(&Constructor::cicle,        constructor_, _1)]
                   | each_to_each [boost::bind(&Constructor::each_to_each, constructor_, _1)]
                   ;
        net = +expression;
    }

    qi::rule<I, ascii::space_type> net;
    qi::rule<I, ascii::space_type> expression;

    os_table pc_os;
    qi::rule<I, std::string(), ascii::space_type> pc_name;
    qi::rule<I, pc(), ascii::space_type> pc_object;

    typedef qi::rule<I, std::vector<pc>(), ascii::space_type> operator_rule;
    operator_rule pc_group;

    operator_rule chain;
    operator_rule star;
    operator_rule cicle;
    operator_rule each_to_each;

    Constructor *constructor_;
};

