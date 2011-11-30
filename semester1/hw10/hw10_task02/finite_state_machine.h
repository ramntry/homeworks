#pragma once

const int init = 0;
const int OK = 8;
enum SymbolType { terminator, sign, digit, point, exp, error };

const char move[5][8] =
{//   0  1  2  3  4  5  6  7   -  Current state
    { 0, 0, 8, 0, 8, 0, 0, 8 } // Terminator
,   { 1, 0, 0, 0, 0, 6, 0, 0 } // Sign
,   { 2, 2, 2, 4, 4, 7, 7, 7 } // Digit
,   { 0, 0, 3, 0, 0, 0, 0, 0 } // Point
,   { 0, 0, 5, 0, 5, 0, 0, 0 } // Exp
};

bool run(const char *word);
