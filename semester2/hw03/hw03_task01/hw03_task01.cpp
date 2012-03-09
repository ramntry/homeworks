#include <iostream>
#include <iomanip>
#include <string>
#include "comparator.h"

using namespace std;

void comparatorsTest()
{
    cout << "(5, 2):\t\t\t"                          << StandartComparator<int>()(5, 2) << endl;
    cout << "<string>(\"hi\", \"hi\"):\t\t"          << StandartComparator<string>()("hi", "hi") << endl;
    cout << "<const char*>(\"hello\", \"world\"):\t" << StandartComparator<const char*>()("hello", "world") << endl;

    StandartComparator<double> *comparators[3];
    comparators[0] = new StandartComparator<double>;
    comparators[2] = new StandartComparator<double>(0.001);
    comparators[1] = new StandartComparator<double>(0.01);
    const int comparatorsLength = 3;

    for (int i = 0; i < comparatorsLength; i++)
        cout << "(" << comparators[i]->eps() << ")"
             << "(2./3., 2.01/3.):\t" << (*comparators[i])(2./3., 2.01/3.) << endl;

    for (int i = 0; i < comparatorsLength; i++)
        delete comparators[i];
}

int main()
{
    comparatorsTest();

    return 0;
}
