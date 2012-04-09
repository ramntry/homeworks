#include <QtCore/QDebug>
#include "stackmachine_qttest.h"

int stackMachineTestExec(int argc, char **argv)
{
    int ret = stackTestExec(argc, argv);

    qDebug() << "\n\n\t\tStackMachine\n";
    StackMachineTest smt;
    return ret | QTest::qExec(&smt, argc, argv);
}
