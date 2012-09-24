#pragma once
#include <gtest/gtest.h>
#include "../src/PersonalComputer.h"

class PersonalComputerTest : public ::testing::Test
{
protected:
    virtual void SetUp();
    virtual void TearDown();

    PersonalComputer *mComputer;
};

