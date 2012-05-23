#include "QtGui/QGridLayout"
#include "TicTacToeWidget.h"
#include "QtGui/QPushButton"

TicTacToeWidget::TicTacToeWidget(QWidget *parent)
    : QWidget(parent)
    , buttonsMapper(new QSignalMapper)
    , engine(new TicTacToe)
    , endMessage(new QMessageBox(QMessageBox::Question, tr("Game over"),
                                 "", QMessageBox::Ok | QMessageBox::Cancel))
{
    setWindowTitle(tr("Tic Tac Toe"));
    QGridLayout* mainLayout = new QGridLayout;
    for (int i = 0; i < fieldSize; ++i)
        for (int j = 0; j < fieldSize; ++j)
        {
            QPushButton* b = new QPushButton;
            b->setMinimumSize(40, 40);
            b->setMaximumSize(40, 40);
            mainLayout->addWidget(b, i, j);
            buttonsMapper->setMapping(b, i * fieldSize + j);
            connect(b, SIGNAL(clicked()), buttonsMapper, SLOT(map()));
        }
    setLayout(mainLayout);
    connect(buttonsMapper, SIGNAL(mapped(int)),
            this, SLOT(step(int)));
}

// Обработчик нажатия одной из кнопок поля
void TicTacToeWidget::step(int cell)
{   // Выполнить ход и установить метку нажатой кнопки в верное значение
    QPushButton* activeButton = dynamic_cast<QPushButton*>(buttonsMapper->mapping(cell));
    if (engine->step(cell) == TicTacToe::X)
        activeButton->setText("X");
    else
        activeButton->setText("O");

    TicTacToe::State result = engine->result();
    if (result != TicTacToe::Undef) // Если игра завершилась
    {
        if (result == TicTacToe::X)
            endMessage->setText("X wins. Start a new game?");
        else if (result == TicTacToe::O)
            endMessage->setText("O wins. Start a new game?");
        else
            endMessage->setText("Tie. Start a new game?");

        if (endMessage->exec() == QMessageBox::Ok)
            reset();
        else
            exit(0);
    }
}

void TicTacToeWidget::reset()
{
    engine->reset();
    for (int i = 0; i < fieldSize * fieldSize; ++i)
        dynamic_cast<QPushButton*>(buttonsMapper->mapping(i))->setText("");
}
