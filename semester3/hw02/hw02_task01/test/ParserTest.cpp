#include "ParserTest.h"

void ParserTest::SetUp()
{
}

void ParserTest::TearDown()
{
}

TEST_F(ParserTest, ParseFullNetconfTest)
{
    EXPECT_TRUE(netconf_parse("../test/netconf/full.netconf"));
}

