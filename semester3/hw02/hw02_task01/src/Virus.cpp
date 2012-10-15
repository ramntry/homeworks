#include <iostream>
#include "Network.h"
#include "PersonalComputer.h"
#include "Virus.h"

int Virus::idCounter_ = 0;

Virus::Virus(OS os)
    : Program(os)
    , id_(idCounter_++)
{
}

void Virus::run(PersonalComputer *computer)
{
    std::cout
        << "Virus (os: " << os_to_string(os())
        << ", id: " << id_ << ", copies: " << numOfCopies() << ")"
        << " is running on \"" << computer->name() << "\" pc" << std::endl;

    Network *nw = computer->network();
    Network::NeighborsRange nbs = nw->neighbors(computer->address());
    for (; nbs.first != nbs.second; ++nbs.first)
    {
        nw->sendMessageTo(shared_from_this(), *nbs.first);
    }
}

int Virus::numOfCopies()
{
    return static_cast<int>(shared_from_this().use_count() - 1);
}

