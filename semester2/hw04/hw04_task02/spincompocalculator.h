#ifndef SPINCOMPOCALCULATOR_H
#define SPINCOMPOCALCULATOR_H

#include <QWidget>

namespace Ui {
    class SpinCompoCalculator;
}

class SpinCompoCalculator : public QWidget
{
    Q_OBJECT

public:
    explicit SpinCompoCalculator(QWidget *parent = 0);
    ~SpinCompoCalculator();

private slots:
    void refreshResult();

private:
    Ui::SpinCompoCalculator *ui;
};

#endif // SPINCOMPOCALCULATOR_H
