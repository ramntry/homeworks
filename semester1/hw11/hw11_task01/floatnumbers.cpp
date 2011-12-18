#include <cctype>
#include <cstdio>
#include "floatnumbers.h"

SymbolType next(FILE *stream, int &current)
{
    current = getc(stream);
    switch (current)
    {
    case '+': case '-':
        return sign;
    case '.':
        return point;
    case 'e': case 'E':
        return exp;
    default:
        if (isdigit(current))
            return digit;
        return other;
    }
}

bool floatNumber(FILE *stream)
{
    static int current = 0;
    for (int state = init;;)
    {
        SymbolType symbol = next(stream, current);
        state = move[symbol][state];
        if (state == init || state == OK)
        {
            ungetc(current, stream);
            return state;
        }
    }
}
