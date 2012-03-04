#include "simpleparser.h"

SimpleParser::SimpleParser()
    : mValue(0.0)
    , mDotHasBeen(false)
    , mIsEmpty(false)
    , mAfterDotMultiplier(1.0)
{}

double SimpleParser::getValue()
{
    mIsEmpty = true;

    return mValue;
}
/*
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
            mValue += mAfterDotMultiplier * num * (mValue > 0 ? 1 : -1);
        }
        emit valueChanged(mValue);
    }
}
*/

void SimpleParser::putNumber(int num)
{
    mStr = mStr + QString::number(num);
    emit valueChanged(mStr);
}


void SimpleParser::cancel()
{
    mValue = 0;
    mIsEmpty = true;
    mDotHasBeen = false;
    mAfterDotMultiplier = 1.0;
    emit valueChanged(0);

}
