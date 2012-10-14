#include <vector>
#include <iostream>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/foreach.hpp>
#include <boost/bind.hpp>

#include <custom/support/read_from_file.hpp>
#include "../OS.h"

namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

struct pc
{
    OS os;
    std::string name;
};

BOOST_FUSION_ADAPT_STRUCT(pc,
    (OS, os)
    (std::string, name)
);

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
struct netconf_parser : qi::grammar<I, ascii::space_type>
{
    netconf_parser(Constructor *constructor)
        : netconf_parser::base_type(net, "netconf-parser")
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


bool netconf_parse(std::string const &filename)
{
    typedef std::string::const_iterator iterator;
    std::string input(read_from_file(filename));
    iterator it = input.begin();
    iterator end = input.end();

    Constructor constructor;
    netconf_parser<iterator> parser(&constructor);

    bool ok = qi::phrase_parse(it, end, parser, ascii::space) && it == end;

    if (!ok) {
        std::cerr << "parsing failed at:\n\t"
                  << std::string(it, std::find(it, end, '\n')) << std::endl;
    }
    return ok;
}

