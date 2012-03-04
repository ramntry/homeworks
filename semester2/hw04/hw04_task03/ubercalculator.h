#pragma once
#include <QtGui/QWidget>
#include <QtGui/QPushButton>
#include <QtGui/QLineEdit>
#include <QtGui/QLayout>
#include <QSignalMapper>
#include "simpleparser.h"

class UberButton : public QPushButton
{
    Q_OBJECT

public:
    UberButton(QString label = "", QWidget *parent = 0)
        : QPushButton(label, parent)
    {
        setMinimumSize(50, 40);
    }
};

class UberCalculator : public QWidget
{
    Q_OBJECT
    
public:
    UberCalculator(QWidget *parent = 0);
    ~UberCalculator();

public slots:
    void testDisplay(QString str);
    void displayOperand(double operand);

private:
    void createOperandsButtons(QGridLayout *placeHere);
    void createOperationsButtons(QGridLayout *placeHere);

    QLineEdit *displayLine;

    QSignalMapper *operandsButtons;   /// 0-9, ~ (+/-), . (decimal point)
    QSignalMapper *operationsButtons; /// +, -, *, ^, /, =
    UberButton *cancelButton;         /// C
    UberButton *cancelOperandButton;  /// CE

    SimpleParser *parser;
};
