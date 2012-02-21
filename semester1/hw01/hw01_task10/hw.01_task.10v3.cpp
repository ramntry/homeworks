// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 01, task 10

// Реализовать программу, проверяющую,
// является ли строка палиндромом.

// [time start] 20:54 19.09.11
// [time estimate] 00:35 00


#include <iostream>
#include <string>

using namespace std;

bool isPalindrome(const string str, int len)
{
    int i = 0;
    len--;
    while (len - i > 0 && str[i] == str[len])  // [v3] v2 тут выполняет
    {                                          // полный бред [/v3]
        len--;
        i++;
    }
    if (len - i < 1)
        return true;
    return false;
}

int main()
{
    clog << "This program determines if a "
         << "string is a palindrome\nType a string: ";
    char ch = cin.get();
    string str;
    while (cin && ch != '\n')
    {
        if (!isspace(ch))
            str.push_back(tolower(ch));
        ch = cin.get();
    }
    
    if (isPalindrome(str, str.length()))
        cout << "String is a palindrome" << endl;
    else
        cout << "String is NOT a palindrome" << endl;

    return 0;
}


// [time done] 21:44 19.09.11
// [time real] 00:50 00

