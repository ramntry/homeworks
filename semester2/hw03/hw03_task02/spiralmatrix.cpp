#include "spiralmatrix.h"

SpiralMatrix::SpiralMatrix(int **m, int size)
    : mMatrix(m)
    , mLinear(0)
    , mSize(size)
{
    if (size % 2 == 0 || size < 0)
        throw std::invalid_argument("The matrix must have positive odd order");
}

SpiralMatrix::~SpiralMatrix()
{
    delete mLinear;
}

void SpiralMatrix::flush()
{
    delete mLinear;
    mLinear = 0;
}

std::ostringstream *SpiralMatrix::getLinear()
{
    if (mLinear)
        return mLinear;

    mLinear = new std::ostringstream;
    printSpiralMatrix(*mLinear);
    return mLinear;
}

void SpiralMatrix::printSpiralMatrix(std::ostream &os)
{
    int y = mSize / 2;
    int x = y;
    int k = 1;
    for (;;)
    {
        for (int i = 0; i < k; i++)
        {
           os << mMatrix[y][x] << ' ';
           if (k % 2)
           {
               y--;
               if (y == -1)
                   return;
           }
           else
               y++;
        }

        for (int j = 0; j < k; j++)
        {
           os << mMatrix[y][x] << ' ';
           if (k % 2)
               x++;
           else
               x--;
        }

        k++;
    }
}

std::ostream &operator <<(std::ostream &os, SpiralMatrix &sp)
{
    return os << sp.getLinear()->str();
}
