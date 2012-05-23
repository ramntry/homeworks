#include "tictactoe_qttest.h"

int ticTacToeTestExec(int argc, char **argv)
{
    TicTacToeTest tt;
    return QTest::qExec(&tt, argc, argv);
}
