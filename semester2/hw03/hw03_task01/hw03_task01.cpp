#include <iostream>
#include "tests.h"

using namespace std;

int main()
{
    cout << "\t\t\tTesting of comparators:\n" << endl;
    comparatorsTest();

    cout << "\n\n\t\t\tTesting of sorters:\n" << endl;
    sortersTest();

    cout << "\n\n\t\t\tTesting of pthread:\n" << endl;
    pthreadTest();

    return 0;
}
