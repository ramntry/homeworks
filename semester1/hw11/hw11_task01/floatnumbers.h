#pragma once
#include <cstdio>

const int init = 0;
const int OK = 8;
enum SymbolType { other, sign, digit, point, exp };

const char move[5][8] =
{//   0  1  2  3  4  5  6  7   -  Current state
    { 0, 0, 8, 0, 8, 0, 0, 8 } // Other
,   { 1, 0, 8, 0, 8, 6, 0, 8 } // Sign
,   { 2, 2, 2, 4, 4, 7, 7, 7 } // Digit
,   { 0, 0, 3, 0, 0, 0, 0, 0 } // Point
,   { 0, 0, 5, 0, 5, 0, 0, 0 } // Exp
};

bool floatNumber(FILE *stream);
