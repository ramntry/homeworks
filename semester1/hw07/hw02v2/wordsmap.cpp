#include <cstdio>
#include <cctype>
#include <cstring>
#include "wordsmap.h"

inline void strToLower(char *buf)
{
    for (int i = 0; (buf[i] = tolower(buf[i])); i++);
}

inline
FILE *openFile(const char *filename)
{
    FILE *f = fopen(filename, "r");
    if (!f)
        throw FileNotFoundException();
    return f;
}

inline
long int getFileSize(FILE *f)
{
    fseek(f, 0, SEEK_END);
    long int size = ftell(f);
    fseek(f, 0, SEEK_SET);
    return size;
}

char *initMap(FILE *f, char *pool)
{
    char *buf  = (char *) malloc(2048);
    buf[0] = '\0';
    fscanf(f, "%*[^-0-9A-Za-z_$@+&]");
    fscanf(f, "%[-0-9A-Za-z_$@+&]", buf);

    char *cursor = pool;
    *(cursor++) = strlen(buf);
    *(cursor++) = 0;
    *(cursor++) = '\0';

    int i = 0;
    for (; buf[i] && i < 255; i++)
        cursor[i] = buf[i];
    cursor[i] = '\0';
    cursor += i + 1;

    free(buf);
    return cursor;
}

WordsMap::WordsMap(const char *filename, bool isDump)
{
    FILE *f = openFile(filename);
    long int size = getFileSize(f);

    if (isDump)
    {
        m_size = size;
        m_map = (char *) malloc(size);
        fread(m_map, 1, size, f);
        m_cursor = m_map + 2;
        m_map[size - 1] = '\0';
        m_map[size - 2] = '\0';
    }
    else
    {
        char *pool = (char *) malloc(5 + size);
        char *cursor = initMap(f, pool);


        fscanf(f, "%*[^-0-9A-Za-z_$@+&]");
        while (fscanf(f, "%[-0-9A-Za-z_$@+&]", cursor) != EOF)
        {
            unsigned int len = strlen(cursor);
            len = len > 255 ? 255 : len;
            *(cursor - 1) = len;
            cursor += len + 1;
            fscanf(f, "%*[^-0-9A-Za-z_$@+&]");
        }
        *cursor = '\0';

        m_size = cursor - pool + 1;
        m_map = (char *) realloc(pool, m_size);
        m_cursor = m_map + 2;
        strToLower(m_cursor + 1);
    }

    fclose(f);
}

char *WordsMap::getWord()
{
    unsigned char lenNext = *(m_cursor - 2);
    m_cursor += *(m_cursor - 1) + 1;
    *(m_cursor - 1) = lenNext;
    *(m_cursor - 2) =  *(m_cursor + lenNext);
    *(m_cursor + lenNext) = '\0';

    return m_cursor;
}

void WordsMap::dump(const char *filename)
{
    FILE *f = fopen(filename, "w");
    fwrite(m_map, 1, m_size, f);
    fclose(f);
}
