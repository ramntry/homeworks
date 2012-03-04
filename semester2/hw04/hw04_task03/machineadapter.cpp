#include "machineadapter.h"

MachineAdapter::MachineAdapter(SimpleParser *parser)
    : mParser(parser)
    , mMachine(new StackMachine())
    , mOperationHasBeen(false)
{
}

void MachineAdapter::putOperation(QString operation)
{
    mMachine->put(mParser->getValue());

    if (mOperationHasBeen)
    {
        mMachine->put(mOperation);
        emit valueChanged(mMachine->getValue());
    }
    else
        mOperationHasBeen = true;

    mOperation = operation.at(0).toAscii();
}

MachineAdapter::~MachineAdapter()
{
    delete mMachine;
}
