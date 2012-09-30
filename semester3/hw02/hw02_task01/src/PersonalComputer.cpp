#include "PersonalComputer.h"

PersonalComputer::PersonalComputer(std::string const &name, OS os)
    : mName(name)
    , mOS(os)
{
}

PersonalComputer::PersonalComputer(OS os)
    : mOS(os)
{
}

std::string PersonalComputer::name()
{
    return mName;
}

OS PersonalComputer::os()
{
    return mOS;
}

void PersonalComputer::assumeNetworkMessage(Program::Pointer program)
{
    if (program->os() == os())
    {
        mPrograms.push_back(program);
    }
}

void PersonalComputer::runAllPrograms()
{
    ProgramIterator end = mPrograms.end();
    for (ProgramIterator it = mPrograms.begin(); it != end; ++it)
    {
        (*it)->run(this);
    }
}

