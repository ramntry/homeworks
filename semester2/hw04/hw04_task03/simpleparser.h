#pragma once
#include <QObject>

enum SpecialOperands { changeSign = 11, decimalDot };

class SimpleParser : public QObject
{
    Q_OBJECT

public:
    SimpleParser();

    double getValue();
    bool isEmpty() { return mIsEmpty; }

public slots:
    void putNumber(int num);
    void cancel();

signals:
    void valueChanged(double value);

private:
    double mValue;
    bool mDotHasBeen;
    bool mIsEmpty;
    double mAfterDotMultiplier;
};
