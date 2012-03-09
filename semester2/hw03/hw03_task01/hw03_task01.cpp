#include <iostream>
#include <string>
#include "comparator.h"

using namespace std;

int main()
{
    cout << StandartComparator<int>()(5, 2) << endl;
    cout << StandartComparator<string>()("hi", "hi") << endl;
    cout << StandartComparator<const char*>()("hello", "world") << endl;

    Comparator<double> *comparators[3];
    comparators[0] = new StandartComparator<double>;
    comparators[1] = new StandartComparator<double>(0.01);
    comparators[2] = new StandartComparator<double>(0.001);
    const int comparatorsLength = 3;

    for (int i = 0; i < comparatorsLength; i++)
        cout << (*comparators[i])(2.0/3.0, 2.01/3.01) << endl;

    for (int i = 0; i < comparatorsLength; i++)
        delete comparators[i];

    return 0;
}
