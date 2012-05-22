#pragma once
#include <queue>
#include <iostream>
#include "../stretchablestack.h"
#include "../oper.h"

typedef std::queue<Oper> OperQueue;
typedef StretchableStack<Oper> OperStack;

int polishMachine(OperQueue &queue, std::istream& in = std::cin);
