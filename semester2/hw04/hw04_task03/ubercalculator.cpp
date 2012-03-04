#include "ubercalculator.h"
#include <QtGui/QLayout>

UberCalculator::UberCalculator(QWidget *parent)
    : QWidget(parent)

    , displayLine(new QLineEdit())

    , operandsButtons(new QSignalMapper())
    , operationsButtons(new QSignalMapper())
    , cancelButton(new UberButton("C"))
    , cancelOperandButton(new UberButton("CE"))
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

    connect(operandsButtons, SIGNAL(mapped(int)), this, SLOT(testDisplay(int)));
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
    
}

void UberCalculator::testDisplay(int num)
{
    displayLine->setText(QString::number(num));
}

void UberCalculator::testDisplay(QString str)
{
    displayLine->setText(str);
}
