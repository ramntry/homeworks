#pragma once
#include <vector>
#include <boost/smart_ptr.hpp>
#include "PersonalComputer.h"

class VirtualServer
{
public:
    typedef boost::shared_ptr<PersonalComputer> PCPtr;
    typedef std::vector<PCPtr> PCContainer;
    typedef PCContainer::iterator PCIterator;

    PersonalComputer *addPC(std::string const &name, OS os);
    PCIterator PCBegin();
    PCIterator PCEnd();
    void compute();

private:
    PCContainer pcs_;
};

