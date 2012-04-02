#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>
#include "Set.h"

using namespace std;

struct IgnoreCaseStringComparator
{
    int operator ()(string const& a, string const& b)
    {
        size_t commonSize = min(a.size(), b.size());
        size_t i = 0;
        while (i < commonSize && tolower(a[i]) == tolower(b[i]))
            ++i;

        if (i == commonSize)
            return comp(a.size(), b.size());
        return comp(tolower(a[i]), tolower(b[i]));
    }

    StandartComparator<int> comp;
};

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
