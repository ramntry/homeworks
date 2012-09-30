#pragma once
#include <boost/enable_shared_from_this.hpp>
#include "OS.h"

class PersonalComputer;

class Program : public boost::enable_shared_from_this<Program>
{
public:
    typedef boost::shared_ptr<Program> Pointer;

    Program(OS os = Windows);

    virtual void run(PersonalComputer *computer) = 0;
    OS os();

private:
    OS mOS;
};

