#include <iostream>
#include <string>
#include "Set.h"

using namespace std;

int main(int argc, char **argv)
{
    setTestExec(argc, argv);

    Set<string> fruits;
    Set<string> striped;

    fruits.add("apple");
    fruits.add("watermelon");
    fruits.add("orange");

    striped.add("mattress");
    striped.add("watermelon");
    striped.add("skunk");

    cout << "I know one striped fruit: ";
    striped.setIntersection(fruits).print(cout);
    cout << endl;

    return 0;
}
