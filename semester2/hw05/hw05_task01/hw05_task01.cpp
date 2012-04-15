#include <iostream>
#include <iomanip>
#include <QtCore/QString>
#include "HashTable.h"
#include "QStringHashers.h"

using namespace std;
typedef HashTable<QString> QStrHash;

QStrHash::Hasher *chooseHasher()
{
    const char *names[] = { "QStringHasherFirstChar", "QStringHasherTwoChars", "QStringHasher" };
    size_t nums = sizeof(names)/sizeof(char *);

    int choice = -1;
    for (;;)
    {
        for (size_t i = 0; i < nums; ++i)
            cout << setw(3) << i << ": " << names[i] << endl;
        cout << "\nEnter a number of desired hash-function: ";

        cin >> choice;
        if (cin && choice >= 0 && choice < nums)
            break;

        cout << "\nIncorrect number. Try again:\n" << endl;
        cin.clear();
        cin.ignore();
    }

    switch (choice)
    {
    case 0:
        return new QStringHasherFirstChar;
    case 1:
        return new QStringHasherTwoChars;
    case 2:
        return new QStringHasher;
    };
    return NULL;
}

void help()
{
    cout << "Usage:\n"
         << "\ta <string> - add <string> into hash-table\n"
         << "\td <string> - remove <string> from hash-table\n"
         << "\tf <string> - check <string> in hash-table\n"
         << "\tc          - rechoose hash-function\n"
         << "\tp          - print hash-table statistics\n"
         << "\th          - print this help\n"
         << "\tq          - quit\n"
         << endl;
}

QString getStr()
{
    while (isspace(cin.peek()))
        cin.get();

    string str;
    getline(cin, str);

    return str.c_str();
}

int main(int argc, char **argv)
{
    int ret = hashTableTestExec(argc, argv);
    if (ret)
        return ret;

    cout << "HashTable. Choose hash-function:\n" << endl;
    QStrHash::Hasher *hasher = chooseHasher();
    cout << "\n(enter h for more details)" << endl;

    QStrHash *table = new QStrHash(2);
    table->setHasher(hasher);

    string buf;
    char command = 0;
    while (command != 'q')
    {
        cout << "> ";
        cin >> buf;
        command = buf[0];

        switch(command)
        {
        case 'h':
            help();
            break;

        case 'c':
            delete hasher;
            hasher = chooseHasher();
            table->setHasher(hasher);
            break;

        case 'p':
            cout << endl;
            table->printStat();
            cout << endl;
            break;

        case 'a':
            table->add(getStr());
            break;

        case 'd':
            table->del(getStr());
            break;

        case 'f':
            if (&table->find(getStr()))
                cout << "ok" << endl;
            else
                cout << "fail" << endl;
            break;

        case 'q':
            break;

        default:
            cout << "\nInvalid command. Try again (enter h for more details)\n"
                 << endl;
        }
    }

    delete table;
    delete hasher;

    return 0;
}
