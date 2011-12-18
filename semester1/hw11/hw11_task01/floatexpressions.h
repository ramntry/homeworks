#pragma once
#include "floatnumbers.h"

bool floatExpression(FILE *stream); // ::= floatNumber tail
bool tail(FILE *stream);            // ::= epsilon | operation floatExpression
bool operation(FILE *stream);       // ::= + | - | * | /
