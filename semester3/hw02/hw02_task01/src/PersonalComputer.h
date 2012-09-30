#pragma once
#include <memory>
#include <vector>
#include "Program.h"
#include "NetworkDevice.h"

class PersonalComputer : public NetworkDevice
{
private:
    typedef std::vector<Program> ProgramContainer;

    virtual void assumeNetworkMessage(Program &program);
};

