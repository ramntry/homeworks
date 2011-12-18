#include <cstdio>
#include <cctype>
#include "floatexpressions.h"

char skipSpace(FILE *stream)
{
    int ch = 0;
    do
        ch = getc(stream);
    while (isspace(ch));

    ungetc(ch, stream);
    return ch;
}

bool floatExpression(FILE *stream)
{
    return (skipSpace(stream) != EOF) && floatNumber(stream) && tail(stream);
}

bool tail(FILE *stream)
{                     // epsilon
    return (skipSpace(stream) == EOF) || (operation(stream) && floatExpression(stream));
}

bool operation(FILE *stream)
{
    int ch = getc(stream);
    return ch == '+' || ch == '-' || ch == '*' || ch == '/';
}
