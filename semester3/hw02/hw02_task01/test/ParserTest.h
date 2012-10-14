#pragma once
#include <gtest/gtest.h>
#include "../src/netconf/parser.h"

class ParserTest : public ::testing::Test
{
protected:
   virtual void SetUp();
   virtual void TearDown();
};

