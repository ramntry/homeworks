#include "finite_state_machine.h"

SymbolType next(const char *&word)
{
    char current = *word;
    word++;

    switch (current)
    {
    case '\0':
        return terminator;
    case '+': case '-':
        return sign;
    case '.':
        return point;
    case 'e': case 'E':
        return exp;
    default:
        if ((current >= '0') && (current <= '9'))
            return digit;
        return error;
    }
}

bool run(const char *word)
{
    for (int state = init;;)
    {
        SymbolType symbol = next(word);
        if (symbol == error)
            return false;

        state = move[symbol][state];
        if (state == init)
            return false;
        if (state == OK)
            return true;
    }
}
