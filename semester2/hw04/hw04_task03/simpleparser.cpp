#include "simpleparser.h"

SimpleParser::SimpleParser()
    : mValue(0.0)
    , mDotHasBeen(false)
    , mIsEmpty(true)
    , mAfterDotMultiplier(1.0)
{}

double SimpleParser::getValue()
{
    mIsEmpty = true;
    return mValue;
}

void SimpleParser::putNumber(int num)
{
    if (num == decimalDot)
        mDotHasBeen = true;
    else if (num == changeSign)
    {
        mValue = -mValue;
        emit valueChanged(mValue);
    }
    else
    {
        mIsEmpty = false;
        if (!mDotHasBeen)
        {
            mValue *= 10.0;
            mValue += num;
        } else {
            mAfterDotMultiplier /= 10.0;
            mValue += mAfterDotMultiplier * num;
        }
        emit valueChanged(mValue);
    }
}

void SimpleParser::cancel()
{
    mValue = 0;
    mIsEmpty = true;
    emit valueChanged(0);
}

