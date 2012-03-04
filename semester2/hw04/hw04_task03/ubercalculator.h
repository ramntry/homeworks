#pragma once
#include <QtGui/QWidget>
#include <QtGui/QPushButton>
#include <QtGui/QLineEdit>
#include <QSignalMapper>

class UberCalculator : public QWidget
{
    Q_OBJECT
    
public:
    UberCalculator(QWidget *parent = 0);
    ~UberCalculator();

private:
    QLineEdit *displayLine;

    QSignalMapper *operandsButtons;   /// 0-9, ~ (+/-), . (decimal point)
    QSignalMapper *operationsButtons; /// +, -, *, ^, /, =
    QPushButton *cancelButton;        /// C
    QPushButton *cancelOperandButton; /// CE
};
