#pragma once
#include <QObject>
#include "simpleparser.h"
#include "stackmachine/stackmachine.h"

class MachineAdapter : public QObject
{
    Q_OBJECT

public:
    MachineAdapter(SimpleParser *parser);
    ~MachineAdapter();

public slots:
    void putOperation(QString operation);

signals:
    void valueChanged(double value);

private:
    SimpleParser *mParser;
    StackMachine *mMachine;
    bool mOperationHasBeen;
    char mOperation;
};
