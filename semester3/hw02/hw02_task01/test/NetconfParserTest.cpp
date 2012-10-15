#include <cstdlib>
#include "NetconfParserTest.h"
#include "../src/Virus.h"

void ParserTest::SetUp()
{
    server_ = new VirtualServer;
    network_ = new Network;
    constructor_ = new Constructor(server_, network_);
    parser_ = new NetconfParser(constructor_);
}

void ParserTest::TearDown()
{
    delete parser_;
    delete constructor_;
    delete server_;
    delete network_;
}

TEST_F(ParserTest, ParseFullNetconfTest)
{
    EXPECT_TRUE(parser_->parse_file("../test/netconf/full.netconf"));

    VirtualServer::PCPtr lin_moscow_srv = *server_->PCBegin();
    network_->sendMessageTo(Program::Pointer(new Virus(Linux)), lin_moscow_srv->address());

    VirtualServer::PCPtr mac_usr_13 = *(--server_->PCEnd());
    network_->sendMessageTo(Program::Pointer(new Virus(MacOS)), mac_usr_13->address());

    for (int i = 0; i < 3; ++i)
    {
        std::cout << "Compute iter No " << i << " >>>" << std::endl;
        server_->compute();
        std::cout << "<<< (" << i << ")" << std::endl;
    }
}

