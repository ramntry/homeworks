#pragma once
#include <sstream>
#include <stdexcept>

class SpiralMatrix
{
public:
    SpiralMatrix(int **matrix, int size);
    ~SpiralMatrix();

    void flush();

protected:
    std::ostringstream *getLinear();
    void printSpiralMatrix(std::ostream &os);

private:
    int **mMatrix;
    std::ostringstream *mLinear;
    int mSize;

friend
    std::ostream &operator <<(std::ostream &os, SpiralMatrix &sp);
};
