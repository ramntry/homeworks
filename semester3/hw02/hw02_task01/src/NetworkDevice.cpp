#include <algorithm>
#include "Network.h"

NetworkDevice::NetworkDevice()
    : mNetworkDeviceWrapper(nullptr)
{
}

NetworkDevice::~NetworkDevice()
{
    unlinkFromNetwork();
}

NetworkAddress NetworkDevice::address()
{
    if (mNetworkDeviceWrapper == nullptr)
    {
        return UndefinedNetworkAddress;
    }
    return mNetworkDeviceWrapper->address();
}

Network *NetworkDevice::network()
{
    if (mNetworkDeviceWrapper == nullptr)
    {
        return nullptr;
    }
    return mNetworkDeviceWrapper->network();
}

void NetworkDevice::unlinkFromNetwork()
{
    Network *nw = network();
    if (nw != nullptr)
    {
        nw->removeDevice(this);
    }
}

void NetworkDevice::setNetworkDeviceWrapper(NetworkDeviceWrapper *wrapper)
{
    mNetworkDeviceWrapper = wrapper;
}

NetworkDeviceWrapper::NetworkDeviceWrapper(Network *network, NetworkDevice *toWrap)
    : mWrapped(toWrap)
    , mNetwork(network)
{
    toWrap->unlinkFromNetwork();
    mWrapped->setNetworkDeviceWrapper(this);
}

NetworkDeviceWrapper::Pointer NetworkDeviceWrapper::create(Network *network
        , NetworkDevice *toWrap)
{
    return Pointer(new NetworkDeviceWrapper(network, toWrap));
}

NetworkDeviceWrapper::~NetworkDeviceWrapper()
{
    if (mWrapped != nullptr)
    {
        mWrapped->setNetworkDeviceWrapper(nullptr);
    }
}

void NetworkDeviceWrapper::setAddress(NetworkAddress const &newAddress)
{
    mAddress = newAddress;
}

NetworkAddress NetworkDeviceWrapper::address()
{
    return mAddress;
}

Network *NetworkDeviceWrapper::network()
{
    return mNetwork;
}

void NetworkDeviceWrapper::addLinkWith(NetworkAddress newNeighbor)
{
    NeighborsIterator end = neighborsEnd();
    if (std::find(neighborsBegin(), end, newNeighbor) == end)
    {
        mNeighborsList.push_back(newNeighbor);
    }
}

void NetworkDeviceWrapper::removeLinkWith(NetworkAddress neighbor)
{
    NeighborsIterator end = neighborsEnd();
    NeighborsIterator finded = std::find(neighborsBegin(), end, neighbor);
    if (finded != end)
    {
        mNeighborsList.erase(finded);
    }
}

NetworkDeviceWrapper::NeighborsIterator NetworkDeviceWrapper::neighborsBegin()
{
    return mNeighborsList.begin();
}

NetworkDeviceWrapper::NeighborsIterator NetworkDeviceWrapper::neighborsEnd()
{
    return mNeighborsList.end();
}

void NetworkDeviceWrapper::assumeNetworkMessage(Program &program)
{
    mWrapped->assumeNetworkMessage(program);
}

