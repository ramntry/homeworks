#include <iostream>
#include "Network.h"
#include "PersonalComputer.h"
#include "Virus.h"

void Virus::run(PersonalComputer *computer)
{
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

