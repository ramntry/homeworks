// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 06

// Заданы две строки: S и S1. Найти
// количество вхождений S1 в S как подстроки.

// [time start] 01:21 20.09.11
// [time estimate] 00:35 00


#include <iostream>
#include <string>

using namespace std;

int substrCounter(const string &pattern, const string &text)
{
    int table[256];
    for (int i = 0; i < 256; i++)
       table[i] = pattern.size(); 
    for (int i = 0; i < pattern.size() - 1; i++)
        table[pattern[i]] = pattern.size() - i - 1;

    int counter = 0;
    int i = pattern.size() - 1;
    int j = i;
    while (i < text.size())
    {
        if (pattern[j] != text[i])
            i += table[text[i]];
        else
        {
            int k = i;
            while (pattern[--j] == text[--k] && j != 0);
            if (j == 0)
            {
                counter++;
                i += pattern.size();
            }
            else
                i++;
            j = pattern.size() - 1;
        }
    }

    return counter;
}

const string getString()
{
    string str;
    char ch = cin.get();
    while (cin && ch != '\n')
    {
        str.push_back(ch);
        ch = cin.get();
    }
    return str;
}
   
int main()
{
    clog << "This program counts the "
         << "number of occurrences of a string Pattern in the Text"
         << endl;
    clog << "Text: ";
    const string text = getString();
    clog << "Pattern: ";
    const string pattern= getString();
    cout << substrCounter(pattern, text) << " occurrences" << endl;
    return 0;
}


// [time done] hh:mm dd.mm.11
// [time real] hh:mm dd

