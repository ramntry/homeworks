#include <iostream>
#include "BinarySearchTree_qttest.h"

void binarySearchTreeTestExec(int argc, char **argv)
{
    BinarySearchTreeTest bstTest;
    QTest::qExec(&bstTest, argc, argv);
    std::cout << "\n\n" << std::endl;
}
