#pragma once
#include <memory>
#include <vector>
#include <string>
#include "Program.h"
#include "OS.h"
#include "NetworkDevice.h"

class PersonalComputer : public NetworkDevice
{
public:
    PersonalComputer(std::string const &name = std::string(), OS os = Windows);
    PersonalComputer(OS os);
    virtual ~PersonalComputer() {}

    std::string name();
    OS os();
    void runAllPrograms();

private:
    typedef std::vector<Program::Pointer> ProgramContainer;
    typedef ProgramContainer::iterator ProgramIterator;

    virtual void assumeNetworkMessage(Program::Pointer program);

    std::string mName;
    OS mOS;
    ProgramContainer mPrograms;
};

