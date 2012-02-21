// author: Roman Tereshin
// email:  roman.tereshin@student.spbu.ru

// hw 05, task 03

// Вывести на консоль все однострочные комментарии С++ (вида //
// комментарий) из входного файла (вместе с символами "//"). До комментария
// в строке может быть значимый текст, его выводить не надо, строки без
// комментариев не выводить.

// [time start] 10:26 19.10.11
// [time estimate] 00:30 00


#include <iostream>
#include <fstream>

using namespace std;

bool printCommentsLine(const string & line)
{
    bool isComment = false;
    bool isString = false;
    for (int i = 1; i < line.size(); i++)
        if (isComment && !isString)
            cout << line[i];
        else if (!isComment && line[i] == '"')
            isString = !isString;
        else if (!isString && line[i] == '/' && line[i - 1] == '/')
        {
            isComment = true;
            cout << "//";
        }
    return isComment;
}

bool printCommentsFile(char * fname)
{
    ifstream f(fname);
    if (!f)
        return false;

    string buf;
    while (getline(f, buf))
        if (printCommentsLine(buf))
            cout << endl;

    return true;
}

int main(int argc, char ** argv)
{
    int i = 1;
    for (; i < argc; i++)
    {
        cout << "\n\nFILE = " << argv[i] << '\n' << endl;
        if (!printCommentsFile(argv[i]))
            cerr << "FILE " << argv[i] << " not found" << endl;
    }

    if (i == 1)
        clog << "This program prints one-line comments start with //\n\n"
             << "Use " << argv[0] << " filename [filename2, ...]" << endl;

    return 0;
}


// [time done] 11:19 19.10.11
// [time real] 00:53 00

