#include "Set_qttest.h"
#include <iostream>

void setTestExec(int argc, char **argv)
{
    binarySearchTreeTestExec(argc, argv);

    SetTest sTest;
    QTest::qExec(&sTest, argc, argv);
    std::cout << "\n\n" << std::endl;
}
