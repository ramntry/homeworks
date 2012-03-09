#include <iostream>
#include "comparator.h"

using namespace std;

int main()
{
    cout << StandartComparator<double>(0.001)(2.0/3.0, 2.01/3.0) << endl;

    return 0;
}
