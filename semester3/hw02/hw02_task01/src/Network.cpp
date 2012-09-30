#include "Network.h"

NetworkAddress Network::mCurrentFreeAddress = 0;

void Network::addDevice(NetworkDevice *newDevice)
{
    NetworkAddress newAddress = getFreeAddress();
    NetworkDeviceWrapper::Pointer wrapper = NetworkDeviceWrapper::create(this, newDevice);
    wrapper->setAddress(newAddress);
    mAddressMap[newAddress] = wrapper;
}

void Network::removeDevice(NetworkDevice *device)
{
    if (device->network() == this)
    {
        NetworkAddress address = device->address();
        for (NeighborsRange nbs = neighbors(address); nbs.first != nbs.second; ++nbs.first)
        {
            mAddressMap[*nbs.first]->removeLinkWith(address);
        }
        mAddressMap.erase(address);
    }
}

bool Network::hasAddress(NetworkAddress address)
{
    return mAddressMap.find(address) != mAddressMap.end();
}

NetworkAddress Network::getFreeAddress()
{
    return mCurrentFreeAddress++;
}

void Network::sendMessageTo(Program::Pointer program, NetworkAddress receiver)
{
    mAddressMap[receiver]->assumeNetworkMessage(program);
}

Network::NeighborsRange Network::neighbors(NetworkAddress address)
{
    NeighborsRange result;
    AddressIterator finded = mAddressMap.find(address);
    if (finded != mAddressMap.end())
    {
        result = NeighborsRange(finded->second->neighborsBegin()
                , finded->second->neighborsEnd());
    }
    return result;
}

void Network::link(NetworkAddress fst, NetworkAddress snd)
{
    AddressIterator fstIt = mAddressMap.find(fst);
    AddressIterator sndIt = mAddressMap.find(snd);
    AddressIterator endIt = mAddressMap.end();
    if (fstIt != endIt && sndIt != endIt)
    {
        fstIt->second->addLinkWith(snd);
        sndIt->second->addLinkWith(fst);
    }
}

