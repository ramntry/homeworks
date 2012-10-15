#pragma once
#include <gtest/gtest.h>
#include "../src/netconf/NetconfParser.h"

class ParserTest : public ::testing::Test
{
protected:
   virtual void SetUp();
   virtual void TearDown();

   VirtualServer *server_;
   Network *network_;
   Constructor *constructor_;
   NetconfParser *parser_;
};

