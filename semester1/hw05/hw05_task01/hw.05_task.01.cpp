// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 05, task 01

// Дан текст, вывести все слова, предварительно оставив в каждом слове
// только первые вхождения каждой буквы.

// [time start] 09:47 19.10.11
// [time estimate] 01:20 00


#include <iostream>
#include <string>

using namespace std;

string & toLower(string & str)
{
    for (int i = 0; i < str.size(); i++)
        str[i] = tolower(str[i]);
    return str;
}

void printUniqLettersWordLt16(const string & word)
{
    for (int i = 0; i < word.size(); i++)
    {
        int j = i - 1;
        for (; j >= 0; j--)
            if (word[i] == word[j])
                break;
        if (j == -1)
            cout << word[i];
    }
}

void printUniqLettersWordGt16(const string & word)
{
    char map[256];
    for (int i = 0; i < 256; i++)
        map[i] = 0;
    for (int i = 0; i < word.size(); i++)
        if (!map[word[i]])
        {
            map[word[i]] = 1;
            cout << word[i];
        }
}

int main()
{
    clog << "This program prints words of the unique character\n"
         << "Enter a string (terminate it with EOF (Ctrl + D):\n";

    string word;
    cin >> word;
    while (cin)
    {
        if (word.size() < 17)
            printUniqLettersWordLt16(toLower(word));
        else
            printUniqLettersWordGt16(toLower(word));
        cout << ' ';
        cin >> word;
    }
    cout << endl;

    return 0;
}


// [time done] 10:26 19.10.11
// [time real] 00:39 00

