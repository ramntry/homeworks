#include <iostream>
#include "PersonalComputer.h"
#include "Virus.h"

void Virus::run(PersonalComputer *computer)
{
    std::cout << "[pc on address: " << computer->address() << "] Virus: run!" << std::endl;
}

