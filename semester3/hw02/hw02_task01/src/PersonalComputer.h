#pragma once
#include <memory>
#include <vector>
#include "Program.h"
#include "NetworkDevice.h"

class PersonalComputer : public NetworkDevice
{
public:
    virtual ~PersonalComputer() {}

    void runAllPrograms();

private:
    typedef std::vector<Program::Pointer> ProgramContainer;
    typedef ProgramContainer::iterator ProgramIterator;

    virtual void assumeNetworkMessage(Program::Pointer program);

    ProgramContainer mPrograms;
};

