#pragma once
#include <memory>
#include <vector>
#include <string>
#include "Program.h"
#include "NetworkDevice.h"

class PersonalComputer : public NetworkDevice
{
public:
    PersonalComputer(std::string const &name = std::string());
    virtual ~PersonalComputer() {}

    std::string name();
    void runAllPrograms();

private:
    typedef std::vector<Program::Pointer> ProgramContainer;
    typedef ProgramContainer::iterator ProgramIterator;

    virtual void assumeNetworkMessage(Program::Pointer program);

    std::string mName;
    ProgramContainer mPrograms;
};

