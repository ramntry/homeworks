#include "PersonalComputer.h"
#include "Program.h"

void PersonalComputer::assumeNetworkMessage(Program::Pointer program)
{
    mPrograms.push_back(program);
}

void PersonalComputer::runAllPrograms()
{
    ProgramIterator end = mPrograms.end();
    for (ProgramIterator it = mPrograms.begin(); it != end; ++it)
    {
        (*it)->run(this);
    }
}

