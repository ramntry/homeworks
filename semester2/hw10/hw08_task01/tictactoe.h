#pragma once

class TicTacToe
{
public:
    static const int fieldSize = 3;
    enum State { X, O, Tie, Undef };  // Элементы этого перечисления будут далее определять
                                      // состояние программы, каждой ячейки поля, исход игры (победителя)
    TicTacToe();

    State step(int cell);  // Выполняет шаг игры при действии на ячейку поля с номером cell (нумерация слева направо
                           // и сверху вниз, начиная с 0). Возвращает X, если ячейка cell по завершению шага содержит
                           // X и O - если O.
    State result()         // Состояние игры - победитель, Tie (ничья) или Undef, если игра еще не завершена.
    { return state; }

    void reset();          // Сбрасывает все внутреннее состояние к начальному.

protected:
    bool checkRow(int row);  // Группа методов, тестирующих поле на возможную победу currPlayer в строке, столбце или
    bool checkCol(int col);  // на диагонали соответственно.
    bool checkDiags();

    int stepCounter;  // Содержит число незаполненных ячеек поля.
    State currPlayer; // Игрок, чей ход ожидается.
    State state;      // см. State result()
    State field[fieldSize][fieldSize];
};

int ticTacToeTestExec(int argc, char **argv);
