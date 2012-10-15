#include "Program.h"

Program::Program(OS os)
    : mOS(os)
{
}

OS Program::os()
{
    return mOS;
}
