#include <boost/foreach.hpp>
#include "VirtualServer.h"

PersonalComputer *VirtualServer::addPC(std::string const &name, OS os)
{
    PCPtr newPC(new PersonalComputer(name, os));
    pcs_.push_back(newPC);
    return newPC.get();
}

VirtualServer::PCIterator VirtualServer::PCBegin()
{
    return pcs_.begin();
}

VirtualServer::PCIterator VirtualServer::PCEnd()
{
    return pcs_.end();
}

void VirtualServer::compute()
{
    BOOST_FOREACH(PCPtr ptr, pcs_) {
        ptr->runAllPrograms();
    }
}

