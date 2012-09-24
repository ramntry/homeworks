#include "PersonalComputerTest.h"

void PersonalComputerTest::SetUp()
{
    mComputer = new PersonalComputer;
}

void PersonalComputerTest::TearDown()
{
    delete mComputer;
}

TEST_F(PersonalComputerTest, CreateTest)
{
    ASSERT_NE(nullptr, mComputer);
}

TEST_F(PersonalComputerTest, NetworkAndAddressOnInitialTest)
{
    EXPECT_EQ(nullptr, mComputer->network());
    EXPECT_EQ(UndefinedNetworkAddress, mComputer->address());
}

