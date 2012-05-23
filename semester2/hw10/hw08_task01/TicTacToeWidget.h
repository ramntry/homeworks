#pragma once
#include <QtGui/QWidget>
#include <QtCore/QSignalMapper>
#include <QtGui/QMessageBox>
#include "tictactoe.h"

class TicTacToeWidget : public QWidget
{
    Q_OBJECT

public:
    static const int fieldSize = 3;

    TicTacToeWidget(QWidget* parent = 0);

public slots:
    void step(int cell);

protected:
    void reset();

    QSignalMapper* buttonsMapper;
    TicTacToe* engine;
    QMessageBox* endMessage;
};
