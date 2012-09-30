#include <vector>
#include <memory>
#include <algorithm>
#include "NetworkTest.h"
#include "../src/Virus.h"
#include "../src/PersonalComputer.h"

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
    EXPECT_EQ(nullptr, computer.network());

    mNetwork->addDevice(&computer);
    EXPECT_NE(UndefinedNetworkAddress, computer.address());
    EXPECT_EQ(mNetwork, computer.network());
    EXPECT_TRUE(mNetwork->hasAddress(computer.address()));
}

TEST_F(NetworkTest, AddAndRemoveByNetworkMethodOnePersonalComputerOnHeapTest)
{
    PersonalComputer *computer = new PersonalComputer;
    mNetwork->addDevice(computer);
    NetworkAddress address = computer->address();
    EXPECT_TRUE(mNetwork->hasAddress(address));

    mNetwork->removeDevice(computer);
    EXPECT_EQ(nullptr, computer->network());
    EXPECT_FALSE(mNetwork->hasAddress(address));
    delete computer;
}

TEST_F(NetworkTest, AddAndRemoveByForceOnePersonalComputerOnHeapTest)
{
    PersonalComputer *computer = new PersonalComputer;
    mNetwork->addDevice(computer);
    NetworkAddress address = computer->address();
    EXPECT_EQ(mNetwork, computer->network());
    EXPECT_TRUE(mNetwork->hasAddress(address));

    delete computer;
    EXPECT_FALSE(mNetwork->hasAddress(address));
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
    SimpleProgram *simpleProgram = new SimpleProgram;

    mNetwork->sendMessageTo(Program::Pointer(simpleProgram), computer.address());
    computer.runAllPrograms();

    EXPECT_EQ(1, simpleProgram->mRunCounter);
}

TEST_F(NetworkTest, ChangeNetworkByPersonalComputerTest)
{
    Network secondaryNetwork;
    PersonalComputer computer;

    mNetwork->addDevice(&computer);
    NetworkAddress address = computer.address();
    EXPECT_TRUE(mNetwork->hasAddress(address));
    EXPECT_FALSE(secondaryNetwork.hasAddress(address));

    secondaryNetwork.addDevice(&computer);
    EXPECT_EQ(&secondaryNetwork, computer.network());
    NetworkAddress newAddress = computer.address();
    EXPECT_NE(newAddress, address);

    EXPECT_FALSE(mNetwork->hasAddress(address));
    EXPECT_FALSE(secondaryNetwork.hasAddress(address));

    EXPECT_FALSE(mNetwork->hasAddress(newAddress));
    EXPECT_TRUE(secondaryNetwork.hasAddress(newAddress));
}

TEST_F(NetworkTest, NeighborsForNonexistingAddressTest)
{
    Network::NeighborsRange neighbors = mNetwork->neighbors(UndefinedNetworkAddress);
    EXPECT_EQ(neighbors.second, neighbors.first);
}

TEST_F(NetworkTest, NeighborsForPCWithoutNeighborsTest)
{
    PersonalComputer pc1;
    PersonalComputer pc2;
    mNetwork->addDevice(&pc1);
    mNetwork->addDevice(&pc2);

    Network::NeighborsRange neighbors1 = mNetwork->neighbors(pc1.address());
    EXPECT_EQ(neighbors1.second, neighbors1.first);

    Network::NeighborsRange neighbors2 = mNetwork->neighbors(pc2.address());
    EXPECT_EQ(neighbors2.second, neighbors2.first);
}

TEST_F(NetworkTest, LinkTwoPCTest)
{
    PersonalComputer pc1;
    PersonalComputer pc2;
    mNetwork->addDevice(&pc1);
    mNetwork->addDevice(&pc2);

    mNetwork->link(pc1.address(), pc2.address());

    Network::NeighborsRange neighbors1 = mNetwork->neighbors(pc1.address());
    EXPECT_EQ(pc2.address(), *neighbors1.first++);
    EXPECT_EQ(neighbors1.second, neighbors1.first);

    Network::NeighborsRange neighbors2 = mNetwork->neighbors(pc2.address());
    EXPECT_EQ(pc1.address(), *neighbors2.first++);
    EXPECT_EQ(neighbors2.second, neighbors2.first);
}

TEST_F(NetworkTest, LinkThreePCsInCircleAndRemoveByForceOneOfThemTest)
{
    int const numOfPCs = 3;

    typedef std::shared_ptr<PersonalComputer> PCPtr;
    typedef std::vector<PCPtr> PCs;
    typedef Network::NeighborsRange Range;

    PCs pcs;
    pcs.push_back(PCPtr(new PersonalComputer));

    mNetwork->addDevice(pcs[0].get());
    for (int i = 1; i < numOfPCs; ++i)
    {
        pcs.push_back(PCPtr(new PersonalComputer));
        mNetwork->addDevice(pcs[i].get());
        mNetwork->link(pcs[i - 1]->address(), pcs[i]->address());
    }
    mNetwork->link(pcs[numOfPCs - 1]->address(), pcs[0]->address());

    for (int i = 0; i < numOfPCs; ++i)
    {
        Range nbs = mNetwork->neighbors(pcs[i]->address());
        EXPECT_EQ(numOfPCs - 1, nbs.second - nbs.first);
        EXPECT_NE(nbs.second, std::find(nbs.first, nbs.second, pcs[(i + 1) % numOfPCs]->address()));
    }

    pcs.erase(pcs.begin());
    int numOfPCsAfterRemoveOneOfThem = pcs.size();

    for (int i = 0; i < numOfPCsAfterRemoveOneOfThem; ++i)
    {
        Range nbs = mNetwork->neighbors(pcs[i]->address());
        EXPECT_EQ(numOfPCsAfterRemoveOneOfThem - 1, nbs.second - nbs.first);
        EXPECT_NE(nbs.second, std::find(nbs.first, nbs.second, pcs[(i + 1) % numOfPCsAfterRemoveOneOfThem]->address()));
    }
}

TEST_F(NetworkTest, LinkFourPCsInCircleSpreadVirusAndRemoveByForceThreePCsTest)
{
    int const numOfPCs = 4;

    typedef std::shared_ptr<PersonalComputer> PCPtr;
    typedef std::vector<PCPtr> PCs;

    PCs pcs;
    pcs.push_back(PCPtr(new PersonalComputer));

    mNetwork->addDevice(pcs[0].get());
    for (int i = 1; i < numOfPCs; ++i)
    {
        pcs.push_back(PCPtr(new PersonalComputer));
        mNetwork->addDevice(pcs[i].get());
        mNetwork->link(pcs[i - 1]->address(), pcs[i]->address());
    }
    mNetwork->link(pcs[numOfPCs - 1]->address(), pcs[0]->address());

    Virus *virus = new Virus;

    mNetwork->sendMessageTo(Program::Pointer(virus), pcs.back()->address());
    EXPECT_EQ(1, virus->numOfCopies());

    pcs.back()->runAllPrograms();
    EXPECT_EQ(3, virus->numOfCopies());

    pcs.erase(pcs.begin());
    EXPECT_EQ(2, virus->numOfCopies());

    pcs.erase(--pcs.end());
    EXPECT_EQ(1, virus->numOfCopies());

    pcs.back()->runAllPrograms();
    EXPECT_EQ(2, virus->numOfCopies());

    pcs.erase(pcs.begin());
    EXPECT_EQ(1, virus->numOfCopies());
}

TEST_F(NetworkTest, LinkThreePCWithDifferentOSsSequentiallyAndSpreadVirusTest)
{
    PersonalComputer root;
    PersonalComputer windows;
    PersonalComputer linux(Linux);

    mNetwork->addDevice(&root);
    mNetwork->addDevice(&windows);
    mNetwork->addDevice(&linux);

    mNetwork->link(root.address(), windows.address());
    mNetwork->link(root.address(), linux.address());

    Virus *virus = new Virus;
    mNetwork->sendMessageTo(Program::Pointer(virus), root.address());
    EXPECT_EQ(1, virus->numOfCopies());

    root.runAllPrograms();
    EXPECT_EQ(2, virus->numOfCopies());
}

