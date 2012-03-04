#pragma once
#include <QObject>
#include <QString>

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
    void valueChanged(QString value);

private:
    double mValue;
    bool mDotHasBeen;
    bool mIsEmpty;
    double mAfterDotMultiplier;

    QString mStr;
};
