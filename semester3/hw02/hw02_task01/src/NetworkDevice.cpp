#include "Network.h"

NetworkDevice::NetworkDevice()
    : mNetworkDeviceWrapper(nullptr)
{
}

NetworkDevice::~NetworkDevice()
{
    Network *nw = network();
    if (nw != nullptr)
    {
        nw->removeDevice(this);
    }
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

void NetworkDevice::setNetworkDeviceWrapper(NetworkDeviceWrapper *wrapper)
{
    mNetworkDeviceWrapper = wrapper;
}

NetworkDeviceWrapper::NetworkDeviceWrapper(Network *network, NetworkDevice *toWrap)
    : mWrapped(toWrap)
    , mNetwork(network)
{
    mWrapped->setNetworkDeviceWrapper(this);
}

NetworkDeviceWrapper::Pointer NetworkDeviceWrapper::create(Network *network, NetworkDevice *toWrap)
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

NetworkDeviceWrapper::AdjacencyIterator NetworkDeviceWrapper::adjacencyBegin()
{
    return mAdjacencyList.begin();
}

NetworkDeviceWrapper::AdjacencyIterator NetworkDeviceWrapper::adjacencyEnd()
{
    return mAdjacencyList.end();
}

void NetworkDeviceWrapper::assumeNetworkMessage(Program *program)
{
    mWrapped->assumeNetworkMessage(program);
}

void NetworkDeviceWrapper::assumeNetworkMessage(Data *data)
{
    mWrapped->assumeNetworkMessage(data);
}

