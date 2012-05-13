#include "Vector_qttest.h"

int vectorTestExec(int argc, char **argv)
{
    VectorTest vt;
    return QTest::qExec(&vt, argc, argv);
}
