#pragma once
#include <memory>

class PersonalComputer;

class Program
{
public:
    typedef std::shared_ptr<Program> Pointer;

    virtual void run(PersonalComputer *computer) = 0;
};

