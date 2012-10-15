#include "NetconfParserTest.h"

void ParserTest::SetUp()
{
    constructor_ = new Constructor;
    parser_ = new NetconfParser(constructor_);
}

void ParserTest::TearDown()
{
    delete parser_;
    delete constructor_;
}

TEST_F(ParserTest, ParseFullNetconfTest)
{
    EXPECT_TRUE(parser_->parse_file("../test/netconf/full.netconf"));
}

