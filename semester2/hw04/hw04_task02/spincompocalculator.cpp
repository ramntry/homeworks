#include "spincompocalculator.h"
#include "ui_spincompocalculator.h"
#include <QMessageBox>

SpinCompoCalculator::SpinCompoCalculator(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::SpinCompoCalculator)
{
    ui->setupUi(this);

    QStringList operations;
    operations << "+" << "-" << "*" << "/";
    ui->operation->addItems(operations);

    connect(ui->operation, SIGNAL(currentIndexChanged(QString)), this, SLOT(refreshResult()));
    connect(ui->leftArg, SIGNAL(valueChanged(int)), this, SLOT(refreshResult()));
    connect(ui->rightArg, SIGNAL(valueChanged(int)), this, SLOT(refreshResult()));
}

SpinCompoCalculator::~SpinCompoCalculator()
{
    delete ui;
}

void SpinCompoCalculator::refreshResult()
{
    int res = 0;
    int left = ui->leftArg->value();
    int right = ui->rightArg->value();

    switch (ui->operation->currentIndex())
    {
        case 0:
            res = left + right;
            break;
        case 1:
            res = left - right;
            break;
        case 2:
            res = left * right;
            break;
        case 3:
            if (right == 0)
                QMessageBox::critical(this, tr("Error"), tr("Division by Zero"));
            else
                res = left / right;
    }
    ui->result->setText(QString::number(res));
}
