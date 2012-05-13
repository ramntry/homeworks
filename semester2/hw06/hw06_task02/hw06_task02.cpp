#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>
#include "Set.h"

using namespace std;

typedef Set<string, IgnoreCaseStringComparator> SetIgnCaseStr;

int main(int argc, char **argv)
{
    setTestExec(argc, argv);

    SetIgnCaseStr smallFruits;
    SetIgnCaseStr bigFruits;
    SetIgnCaseStr striped;

    smallFruits.add("Apple");
    bigFruits.add("Watermelon");
    bigFruits.add("Pineapple");
    smallFruits.add("Orange");

    striped.add("mattress");
    striped.add("watermelon");
    striped.add("skunk");

    cout << "I know one striped fruit: ";
    SetIgnCaseStr fruits = smallFruits.setUnion(bigFruits);
    striped.setIntersection(fruits).print(cout);
    cout << endl;

    return 0;
}
