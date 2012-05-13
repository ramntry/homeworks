#include "Bag.h"
#include <iostream>
#include <string>

typedef Bag<std::string, IgnoreCaseStringComparator> BagIStr;

void toUpper(std::string &str)
{
    for (size_t i = 0; i < str.size(); ++i)
        str[i] = toupper(str[i]);
}

void toLower(std::string &str)
{
    for (size_t i = 0; i < str.size(); ++i)
        str[i] = tolower(str[i]);
}

int main(int argc, char **argv)
{
    int ret = bagTestExec(argc, argv);
    if (ret)
        return ret;
    std::cout << "\n" << std::endl;


    BagIStr fruits;
    fruits << "Mango"  << "Apple" << "Watermelon" << "Cherry"
           << "Orange" << "Apple" << "Banana"     << "Orange"
           << "Apple"  << "Orange";
    std::cout << fruits << std::endl;

    BagIStr::Iterator it = fruits.begin();
    for (int i = 0; it != fruits.end(); ++it, ++i)
        if (i % 2)
            toUpper(*it);
        else
            toLower(*it);
    std::cout << fruits << std::endl;

    std::cout << "Oranges( ";
    BagIStr::Iterator fnd = fruits.find("Orange");
    for (; fnd != fruits.end(); ++fnd)
        std::cout << *fnd << ' ';
    std::cout << ')' << std::endl;

    return 0;
}
