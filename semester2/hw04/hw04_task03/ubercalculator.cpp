#include <QtGui/QLayout>
#include "ubercalculator.h"

UberCalculator::UberCalculator(QWidget *parent)
    : QWidget(parent)

    , displayLine(new QLineEdit("0"))

    , operandsButtons(new QSignalMapper())
    , operationsButtons(new QSignalMapper())
    , cancelButton(new UberButton("C"))
    , cancelOperandButton(new UberButton("CE"))

    , parser(new SimpleParser())
{
    displayLine->setReadOnly(true);
    displayLine->setAlignment(Qt::AlignRight);

    QGridLayout *buttonsLayout = new QGridLayout();

    createOperandsButtons(buttonsLayout);
    createOperationsButtons(buttonsLayout);
    buttonsLayout->addWidget(cancelButton, 0, 3);
    buttonsLayout->addWidget(cancelOperandButton, 0, 4);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addWidget(displayLine);
    mainLayout->addLayout(buttonsLayout);

    setLayout(mainLayout);
    setWindowTitle(tr("Uber Calculator"));

    connect(operandsButtons, SIGNAL(mapped(int)), parser, SLOT(putNumber(int)));
    connect(parser, SIGNAL(valueChanged(double)), this, SLOT(displayOperand(double)));
    connect(cancelOperandButton, SIGNAL(clicked()), parser, SLOT(cancel()));
    connect(operationsButtons, SIGNAL(mapped(QString)), this, SLOT(testDisplay(QString)));
}

void UberCalculator::createOperandsButtons(QGridLayout *placeHere)
{
    QString buttonLabels[] = { "0", "+/-", "." };
    for (int i = 1; i <= decimalDot; i++) // decimalDot == 12
    {
        UberButton *b = new UberButton((i / 10) ? buttonLabels[i - 10] : QString::number(i));
        placeHere->addWidget(b, (i - 1) / 3, (i - 1) % 3);

        operandsButtons->setMapping(b, i == 10 ? 0 : i); // 11 == changeSign
        connect(b, SIGNAL(clicked()), operandsButtons, SLOT(map()));
    }
}

void UberCalculator::createOperationsButtons(QGridLayout *placeHere)
{
    QString buttonLabels[] = { "+", "-", "*", "^", "/", "=" };
    for (int i = 0; i < 6; i++)
    {
        UberButton *b = new UberButton(buttonLabels[i]);
        placeHere->addWidget(b, i / 2 + 1, i % 2 + 3);

        operationsButtons->setMapping(b, buttonLabels[i]);
        connect(b, SIGNAL(clicked()), operationsButtons, SLOT(map()));
    }
}

UberCalculator::~UberCalculator()
{
    delete parser;
}

void UberCalculator::testDisplay(QString str)
{
    displayLine->setText(str);
}

void UberCalculator::displayOperand(double operand)
{
    displayLine->setText(QString::number(operand, 'g', 14));
}
