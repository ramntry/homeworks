#include "ubercalculator.h"
#include <QtGui/QLayout>

UberCalculator::UberCalculator(QWidget *parent)
    : QWidget(parent)

    , displayLine(new QLineEdit())

    , operandsButtons(new QSignalMapper())
    , operationsButtons(new QSignalMapper())
    , cancelButton(new QPushButton("C"))
    , cancelOperandButton(new QPushButton("CE"))
{
    QGridLayout *buttonsLayout = new QGridLayout();


    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addWidget(displayLine);
    mainLayout->addLayout(buttonsLayout);

    displayLine->setReadOnly(true);

    setLayout(mainLayout);
    setWindowTitle(tr("Uber Calculator"));
}

UberCalculator::~UberCalculator()
{
    
}
