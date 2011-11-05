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
    unsigned int getSize() { return m_size; }

private:
    char *m_map;
    char *m_cursor;
    unsigned int m_size;
};
