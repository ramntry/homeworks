#include "ubercalculator.h"
#include <QtGui/QVBoxLayout>

UberCalculator::UberCalculator(QWidget *parent)
    : QWidget(parent)

    , displayLine(new QLineEdit())

    , operandsButtons(new QSignalMapper())
    , operationsButtons(new QSignalMapper())
    , cancelButton(new QPushButton("C"))
    , cancelOperandButton(new QPushButton("CE"))
{
    displayLine->setReadOnly(true);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addWidget(displayLine);

    setLayout(mainLayout);
    setWindowTitle(tr("Uber Calculator"));
}

UberCalculator::~UberCalculator()
{
    
}
