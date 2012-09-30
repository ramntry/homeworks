#pragma once
#include <boost/enable_shared_from_this.hpp>

class PersonalComputer;

class Program : public boost::enable_shared_from_this<Program>
{
public:
    typedef boost::shared_ptr<Program> Pointer;

    virtual void run(PersonalComputer *computer) = 0;
};

