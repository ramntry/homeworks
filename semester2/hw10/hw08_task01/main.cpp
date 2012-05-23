#include <QtGui/QApplication>
#include "TicTacToeWidget.h"

int main(int argc, char **argv)
{
    int ret = ticTacToeTestExec(argc, argv);
    if (ret)
        return ret;

    QApplication app(argc, argv);

    TicTacToeWidget ticTacToe;
    ticTacToe.show();

    return app.exec();
}
