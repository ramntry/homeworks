#include <cstring>
#include <iostream>
#include <vector>
#include "karpa_rabina.h"

using namespace std;

typedef unsigned long long ullong;
const ullong module = 72057594037927931ULL;  // ближайшее к ((2^64 - 1) / 256) простое слева

inline
ullong calcMultiplier(int m)
{
    ullong multiplier = 1;
    for (int i = 1; i < m; i++)
        multiplier = (multiplier * 256) % module;
    return multiplier;
}

ullong calcHash(const char *str, int size)
{
    ullong hash = 0;
    for (int i = 0; i < size; i++)
        hash = ((hash * 256) + str[i]) % module;
    return hash;
}

void refreshHash(ullong &hash, const char *text, int offset, int size, ullong multiplier)
{
    hash -= text[offset] * multiplier;
    hash *= 256;
    hash += text[offset + size];
    hash %= module;
}

bool checkHint(const char *text, const char *pattern, int offset)
{
    int i = 0;
    for (; text[i + offset] && (pattern[i] == text[i + offset]); i++);
    return !pattern[i];
}

vector<int> karpaRabina(const char *text, const char *pattern)
{
    vector<int> offsets;

    int patternSize = strlen(pattern);
    int textSize = strlen(text);

    ullong patternHash = calcHash(pattern, patternSize);
    ullong currentHash = calcHash(text, patternSize);
    ullong multiplier  = calcMultiplier(patternSize);

    for (int i = 0; i < textSize - patternSize + 1; ++i)
    {
        if (currentHash == patternHash)
            if (checkHint(text, pattern, i))
                offsets.push_back(i);

        refreshHash(currentHash, text, i, patternSize, multiplier);
    }

    return offsets;
}
