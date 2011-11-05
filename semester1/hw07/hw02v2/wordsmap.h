#include <cstdlib>
#pragma once

class FileNotFoundException {};

class WordsMap
{
public:
    WordsMap(const char *filename, bool isDump = false);
    ~WordsMap() { free(m_map); }
    char *getWord();
    void dump(const char *filename);

private:
    char *m_map;
    char *m_cursor;
    unsigned int m_size;
};
