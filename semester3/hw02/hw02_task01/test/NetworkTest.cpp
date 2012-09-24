#include "NetworkTest.h"
#include "../src/PersonalComputer.h"
#include "../src/Program.h"

void NetworkTest::SetUp()
{
    mNetwork = new Network;
}

void NetworkTest::TearDown()
{
    delete mNetwork;
}

TEST_F(NetworkTest, CreateTest)
{
    ASSERT_NE(nullptr, mNetwork);
}

TEST_F(NetworkTest, AddOnePersonalComputerOnStackTest)
{
    PersonalComputer computer;
    EXPECT_EQ(UndefinedNetworkAddress, computer.address());

    mNetwork->addDevice(&computer);
    EXPECT_NE(UndefinedNetworkAddress, computer.address());
}

TEST_F(NetworkTest, AddAndRemoveByNetworkMethodOnePersonalComputerOnHeapTest)
{
    PersonalComputer *computer = new PersonalComputer;
    mNetwork->addDevice(computer);
    EXPECT_EQ(mNetwork, computer->network());

    mNetwork->removeDevice(computer);
    EXPECT_EQ(nullptr, computer->network());
    delete computer;
}

TEST_F(NetworkTest, SendToComputerSimpleProgramTest)
{
    PersonalComputer computer;
    mNetwork->addDevice(&computer);

    struct SimpleProgram : public Program
    {
        SimpleProgram() : mRunCounter(0) {}
        virtual void run(PersonalComputer *) { mRunCounter++; }
        int mRunCounter;
    };
    SimpleProgram program;

    mNetwork->sendMessageTo(&program, computer.address());
    EXPECT_EQ(1, program.mRunCounter);
}

