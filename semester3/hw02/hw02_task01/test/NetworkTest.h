#pragma once
#include <gtest/gtest.h>
#include "../src/Network.h"

class NetworkTest : public ::testing::Test
{
protected:
   virtual void SetUp();
   virtual void TearDown();

   Network *mNetwork;
};

