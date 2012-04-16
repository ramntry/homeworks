#include "Bag_qttest.h"

int bagTestExec(int argc, char **argv)
{
    BagTest bt;
    return QTest::qExec(&bt, argc, argv);
}

