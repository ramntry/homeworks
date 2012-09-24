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
        mAddressMap.erase(device->address());
    }
}

NetworkAddress Network::getFreeAddress()
{
    return mCurrentFreeAddress++;
}

void Network::sendMessageTo(Program *program, NetworkAddress receiver)
{
    mAddressMap[receiver]->assumeNetworkMessage(program);
}

