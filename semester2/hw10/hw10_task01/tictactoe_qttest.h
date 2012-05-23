#pragma once
#include <QtTest/QtTest>
#include "tictactoe.h"

class TicTacToeTest : public QObject
{
    Q_OBJECT

private slots:
    void initTestCase()
    {
        testCounter = 0;
        game = NULL;
    }

    void init()
    {
        ++testCounter;
        if (game == NULL)
            game = new TicTacToe;
        else
            game->reset();
    }

    void cleanup()
    {
        if (testCounter % 2 == 0)
        {
            delete game;
            game = NULL;
        }
    }

    void testRow() // with new game
    {
        int script[] = { 0, 3, 1, 6, 2 };
        int states[] = { 0, 1, 0, 1, 0 }; // 0 -> X; 1 -> O
        size_t size = sizeof(script)/sizeof(int);

        playScript(script, states, size);
        QCOMPARE(game->result(), TicTacToe::X);
    }

    void testCol() // with reseted game
    {
        int script[] = { 0, 1, 3, 4, 2, 7 };
        int states[] = { 0, 1, 0, 1, 0, 1 }; // 0 -> X; 1 -> O
        size_t size = sizeof(script)/sizeof(int);

        playScript(script, states, size);
        QCOMPARE(game->result(), TicTacToe::O);
    }

    void testDiag() // with new game
    {
        int script[] = { 0, 1, 4, 5, 8 };
        int states[] = { 0, 1, 0, 1, 0 }; // 0 -> X; 1 -> O
        size_t size = sizeof(script)/sizeof(int);

        playScript(script, states, size);
        QCOMPARE(game->result(), TicTacToe::X);
    }

    void testMultiplePress() // with reseted game
    {
        int script[] = { 4, 4, 4, 1, 1, 4, 2, 0, 6 };
        int states[] = { 0, 0, 0, 1, 1, 0, 0, 1, 0 };
        size_t size = sizeof(script)/sizeof(int);

        playScript(script, states, size);
        QCOMPARE(game->result(), TicTacToe::X);
    }

    void testTie() // with new game
    {
        int script[] = { 4, 1, 8, 0, 2, 5, 7, 6, 3 };
        int states[] = { 0, 1, 0, 1, 0, 1, 0, 1, 0 };
        size_t size = sizeof(script)/sizeof(int);

        playScript(script, states, size);
        QCOMPARE(game->result(), TicTacToe::Tie);
    }

protected:
    void playScript(int* cells, int* states, size_t size)
    {
        for (size_t i = 0; i < size; ++i)
        {
            QCOMPARE(game->result(), TicTacToe::Undef);
            QCOMPARE(game->step(cells[i]), (TicTacToe::State)states[i]);
        }
    }

    TicTacToe* game;
    int testCounter;
};
