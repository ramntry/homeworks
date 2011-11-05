#include <cstdio>
#include <cctype>
#include <cstring>
#include "wordsmap.h"

inline
void strToLower(char *buf)
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

inline
void makeHeader(char *pool)
{
    pool[0] = pool[2];
    pool[1] = 0;
    pool[2] = '\0';
}

inline
void makeTerm(char *pool, long int size)
{
    pool[size - 1] = '\0';
    pool[size - 2] = '\0';
}

/* Формат внутреннего представления следующий:
 *
 * [sf][0]\0first . .
 *         ^       .  [sn][sc]current\0next[s1]string1[s2]string2 ... [sl]last\0\0
 * (заголовок)                ^
 *                                     (центральный фрагмент)                 (терминатор)
 *
 * где [sX] - размер строки, соответственно X: f - first, n - next, c - current etc, \0 - нулевой байт.
 *     [0]  - размер охранника и \0 - сам охранник (строка нулевой длины)
 *     current  - строка, указатель на которую (^, m_cursor) только что передан клиенту методом getWord().
 * Заголовок изображен отдельно, поскольку для строки ниже он не актуален - будет изменен.
 */

WordsMap::WordsMap(const char *filename, bool isDump)
{
    FILE *f = openFile(filename);
    long int size = getFileSize(f);

    if (isDump)
    {
        m_map = (char *) malloc(size);
        m_size = fread(m_map, 1, size, f);
        m_cursor = m_map + 2;              // Установка курсора в боевое положение
        makeTerm(m_map, m_size);           // Защита от подтасовки некорректного файла
    }
    else
    {
        m_map = (char *) malloc(5 + size);
        char *cursor = m_map + 3;

        while ((fscanf(f, noWordRegExp)!= EOF) &&          // По большому счету, огород из-за желания избавиться от
               (fscanf(f, wordRegExp, cursor) != EOF))     // ... предупреждения "проигнорировано значение fscanf"
        {
            unsigned int len = strlen(cursor);
            len = len > 255 ? 255 : len;       // Усечение строк

            *(cursor - 1) = len;               // Запись слева от строки ее размера
            cursor += len + 1;
        }

        m_size = cursor - m_map + 1;
        m_map = (char *) realloc(m_map, m_size);
        m_cursor = m_map + 2;

        makeHeader(m_map);
        makeTerm(m_map, m_size);
        strToLower(m_cursor + 1);
    }
    fclose(f);
}

/**
 * Преобразование текущей позиции в отображении к стандартной null-терминированной строке.
 * ВНИМАНИЕ: Выход за границы дампа не контролируется, лежит на плечах клиентского кода.
 * Признак конца дампа - длина очередной отданной строки - 0 (под возвращенным указателем нулевой байт)
 */
char *WordsMap::getWord()                     // Сначала сдвигаем курсор (потому нужен охранник), потом
{                                             // ... отдаем указатель.
    unsigned char lenNext = *(m_cursor - 2);  // Длина строки, очередной для следующего запроса на два байта левее.
    m_cursor += *(m_cursor - 1) + 1;          // Переход в начало строки, очередной для данного запроса
    *(m_cursor - 1) = lenNext;                // Повторим структуру - запишем длину текущей строки слева от ее головы
    *(m_cursor - 2) =  *(m_cursor + lenNext); // ... и длину следующей строки - еще на байт левее
    *(m_cursor + lenNext) = '\0';             // Перед возвратом стандартизируем строку

    return m_cursor;
}

void WordsMap::dump(const char *filename)
{
    FILE *f = fopen(filename, "w");
    fwrite(m_map, 1, m_size, f);
    fclose(f);
}
