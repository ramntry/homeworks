#pragma once
#include <unordered_map>
#include "NetworkDevice.h"

class Network
{
public:
    typedef std::pair<NetworkDeviceWrapper::NeighborsIterator
            , NetworkDeviceWrapper::NeighborsIterator> NeighborsRange;

    void addDevice(NetworkDevice *newDevice);
    void removeDevice(NetworkDevice *device);
    bool hasAddress(NetworkAddress address);
    void sendMessageTo(Program *program, NetworkAddress receiver);
    NeighborsRange neighbors(NetworkAddress address);
    void link(NetworkAddress fst, NetworkAddress snd);

private:
    typedef std::unordered_map<NetworkAddress, NetworkDeviceWrapper::Pointer> AddressMap;
    typedef AddressMap::iterator AddressIterator;

    NetworkAddress getFreeAddress();

    static NetworkAddress mCurrentFreeAddress;

    AddressMap mAddressMap;
};

