#include "sstream"
#include <QtGui/QLayout>
#include <QtDebug>
#include "ubercalculator.h"

UberCalculator::UberCalculator(QWidget *parent)
    : QWidget(parent)

    , displayLine(new QLineEdit())

    , operandsButtons(new QSignalMapper())
    , operationsButtons(new QSignalMapper())
    , cancelButton(new UberButton("C"))
    , executeButton(new UberButton("exe"))
{
    QGridLayout *buttonsLayout = new QGridLayout();

    createOperandsButtons(buttonsLayout);
    createOperationsButtons(buttonsLayout);
    buttonsLayout->addWidget(cancelButton, 3, 5);
    buttonsLayout->addWidget(executeButton, 3, 4);

    QVBoxLayout *mainLayout = new QVBoxLayout;
    mainLayout->addWidget(displayLine);
    mainLayout->addLayout(buttonsLayout);

    setLayout(mainLayout);
    setWindowTitle(tr("Uber Calculator"));

    connect(operandsButtons, SIGNAL(mapped(QString)),
            this, SLOT(addText(QString)));
    connect(operationsButtons, SIGNAL(mapped(QString)),
            this, SLOT(addText(QString)));
    connect(cancelButton, SIGNAL(clicked()),
            displayLine, SLOT(clear()));
    connect(executeButton, SIGNAL(clicked()),
            this, SLOT(execute()));
    connect(displayLine, SIGNAL(returnPressed()),
            this, SLOT(execute()));
}

void UberCalculator::createOperandsButtons(QGridLayout *placeHere)
{
    QString buttonLabels[] = { "0", ".", ",  " };
    for (size_t i = 1; i < 10 + sizeof(buttonLabels)/sizeof(QString); i++)
    {
        QString label = (i / 10) ? buttonLabels[i - 10] : QString::number(i);
        UberButton *b = new UberButton(label);
        placeHere->addWidget(b, (i - 1) / 3, (i - 1) % 3);

        operandsButtons->setMapping(b, label);
        connect(b, SIGNAL(clicked()), operandsButtons, SLOT(map()));
    }
}

void UberCalculator::createOperationsButtons(QGridLayout *placeHere)
{
    QString buttonLabels[] = { "(", ")", "^", " + ", " - ", "a", "*", "/", "b", " = " };

    for (size_t i = 0; i < sizeof(buttonLabels)/sizeof(QString); i++)
    {
        UberButton *b = new UberButton(buttonLabels[i]);
        placeHere->addWidget(b, i / 3, i % 3 + 3);

        operationsButtons->setMapping(b, buttonLabels[i]);
        connect(b, SIGNAL(clicked()), operationsButtons, SLOT(map()));
    }
}

void UberCalculator::addText(const QString &text)
{
    displayLine->setFocus();
    displayLine->insert(text);
}

void UberCalculator::execute()
{
    std::istringstream in(displayLine->text().toStdString());
    OperQueue operQueue;
    StackMachine stackMachine;

    polishMachine(operQueue, in);
    for (; !operQueue.empty() && stackMachine.isOK(); operQueue.pop())
        stackMachine.put(operQueue.front());

    displayLine->setFocus();
    displayLine->setText(QString::number(stackMachine.getValue()));
}
