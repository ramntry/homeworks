#pragma once
#include "Program.h"

class Virus : public Program
{
public:
    Virus(OS os = Windows);

    virtual void run(PersonalComputer *computer);
    int numOfCopies();

private:
    static int idCounter_;
    int id_;
};

