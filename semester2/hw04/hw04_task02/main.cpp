#include <QtGui/QApplication>
#include "spincompocalculator.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    SpinCompoCalculator w;
    w.show();

    return a.exec();
}
