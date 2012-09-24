#pragma once
#include "NetworkDevice.h"

class PersonalComputer : public NetworkDevice
{
private:
    virtual void assumeNetworkMessage(Program *program);
    virtual void assumeNetworkMessage(Data *data);
};

