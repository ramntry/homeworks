#pragma once
#include <unordered_map>
#include "NetworkDevice.h"

class Network
{
public:
    void addDevice(NetworkDevice *newDevice);
    void removeDevice(NetworkDevice *device);
    void sendMessageTo(Program *program, NetworkAddress receiver);

private:
    typedef std::unordered_map<NetworkAddress, NetworkDeviceWrapper::Pointer> AddressMap;
    typedef AddressMap::iterator AddressIterator;

    NetworkAddress getFreeAddress();

    static NetworkAddress mCurrentFreeAddress;

    AddressMap mAddressMap;
};

