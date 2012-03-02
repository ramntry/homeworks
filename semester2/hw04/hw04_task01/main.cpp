#include <QtGui/QApplication>
#include "sliderandprogressbar.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    SliderAndProgressBar w;
    w.show();

    return a.exec();
}
