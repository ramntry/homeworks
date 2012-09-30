#pragma once
#include <vector>
#include <memory>
#include "NetworkAddress.h"

class Network;
class Program;

class NetworkDeviceWrapper;

class NetworkDevice
{
public:
    NetworkDevice();
    virtual ~NetworkDevice();

    NetworkAddress address();
    Network *network();

protected:
    virtual void assumeNetworkMessage(Program &program) = 0;

    void unlinkFromNetwork();

private:
    void setNetworkDeviceWrapper(NetworkDeviceWrapper *wrapper);

    NetworkDeviceWrapper *mNetworkDeviceWrapper;

friend class NetworkDeviceWrapper;
};

class NetworkDeviceWrapper
{
public:
    typedef std::shared_ptr<NetworkDeviceWrapper> Pointer;
    typedef std::vector<NetworkAddress> NeighborsList;
    typedef NeighborsList::iterator NeighborsIterator;

    NetworkDeviceWrapper(Network *network, NetworkDevice *toWrap);
    static Pointer create(Network *network, NetworkDevice *toWrap);
    ~NetworkDeviceWrapper();

    void setAddress(NetworkAddress const &newAddress);
    NetworkAddress address();
    Network *network();

    void addLinkWith(NetworkAddress newNeighbor);
    void removeLinkWith(NetworkAddress neighbor);
    NeighborsIterator neighborsBegin();
    NeighborsIterator neighborsEnd();

    void assumeNetworkMessage(Program &program);

private:
    NetworkDevice *mWrapped;
    NetworkAddress mAddress;
    Network *mNetwork;
    NeighborsList mNeighborsList;
};

