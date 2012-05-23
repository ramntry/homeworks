#include "tictactoe.h"

TicTacToe::TicTacToe()
    : stepCounter(fieldSize * fieldSize)
    , currPlayer(X) // Первым ход держит X
    , state(Undef)  // Исход игры еще не ясен
{
    for (int i = 0; i < fieldSize; ++i)
        for (int j = 0; j < fieldSize; ++j)
            field[i][j] = Undef;       // в начальный момент времени состояние всего поля не определено.
}

// Может быть, текущий игрок, совершив ход, одержал победу в активной строке поля?
bool TicTacToe::checkRow(int row)
{
    for (int j = 0; j < fieldSize; ++j)
        if (field[row][j] != currPlayer)
            return false;
    return true;
}

// ... или активном столбце?
bool TicTacToe::checkCol(int col)
{
    for (int i = 0; i < fieldSize; ++i)
        if (field[i][col] != currPlayer)
            return false;
    return true;
}

// ... или на одной из диагоналей?
bool TicTacToe::checkDiags()
{
    bool main = true;
    bool second = true;
    for (int d = 0; d < fieldSize; ++d)
    {
        main &= field[d][d] == currPlayer;
        second &= field[d][fieldSize - (d + 1)] == currPlayer;
    }
    return main || second;
}

TicTacToe::State TicTacToe::step(int cell)
{
    int row = cell / fieldSize;
    int col = cell % fieldSize;

    if (field[row][col] != Undef) // При попытке хода на уже занятой ячейке состояние программы не меняется
        return field[row][col];
    field[row][col] = currPlayer; // Иначе актуальная ячейка сохраняет новое значение - X или O,
    --stepCounter;                // а число незанятых ячеек уменьшается на 1.

    // Если в результате хода текущий игрок одержал победу
    if (checkRow(row) || checkCol(col) || checkDiags())
        state = currPlayer;    // перейти в состояние "победу одержал X(O)"
    // или же никто не выиграл, а все ячейки поля оказались заняты:
    else if (stepCounter == 0)
        state = Tie;           // тогда перейти в состояние "ничья"

    State oldPlayer = currPlayer;  // Сменить активного игрока и вернуть актуальное содержимое ячейки
    currPlayer = (State)(1 - currPlayer);
    return oldPlayer;
}

void TicTacToe::reset()
{
    stepCounter = fieldSize * fieldSize;
    state = Undef;
    currPlayer = X;
    for (int i = 0; i < fieldSize; ++i)
        for (int j = 0; j < fieldSize; ++j)
            field[i][j] = Undef;
}
