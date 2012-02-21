// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 04, task 01

// Даны две строки. Определить, можно ли, переставляя
// символы в первой строке, получить вторую строку

// [time start] 15:45 05.10.11
// [time estimate] 00:25 00


#include <iostream>
#include <string>

using namespace std;

bool isAnagramma(const string & first, const string & second)
{
    int size = first.size();
    if (second.size() != size)
        return false;
    int hist[256];
    for (int i = 0; i < 256; i++)
        hist[i] = 0;

    for (int i = 0; i < size; i++)
    {
        ++hist[first[i]];
        --hist[second[i]];
    }

    for (int i = 0; i < 256; i++)
        if (hist[i])
            return false;
    return true;
}

int main()
{
    clog << "This program checks anagramma-strings\n"
         << "First string: ";
    string first;
    getline(cin, first);
    clog << "Second string: ";
    string second;
    getline(cin, second);

    if (isAnagramma(first, second))
        cout << "Strings are anagramma";
    else
        cout << "Strings are NOT anagramma";
    cout << endl;

    return 0;
}


// [time done] 16:02 05.10.11
// [time real] 00:17 00

