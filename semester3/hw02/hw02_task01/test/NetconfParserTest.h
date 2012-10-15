#pragma once
#include <gtest/gtest.h>
#include "../src/netconf/NetconfParser.h"

class ParserTest : public ::testing::Test
{
protected:
   virtual void SetUp();
   virtual void TearDown();

   Constructor *constructor_;
   NetconfParser *parser_;
};

