#include <iostream>
#include <fstream>
#include <string>
#include "spiralmatrix.h"
#include "spiralmatrix_testsupport.h"

using namespace std;

/**
  This function manages the IO-streams specific way this program - asks the user mode - output to screen or
  file, returns the open to write stream. If you pass a non-zero argument is correct closes (removes)
  the flow only. Do not use it twice with a zero argument!
 */
ostream *ouputStreamManage(ostream *pofs = 0)
{
    static bool isFileInHeap = false;
    if (pofs)
    {
        if (isFileInHeap)
        {
            static_cast<ofstream *>(pofs)->close();
            delete pofs;
        }
        return 0;
    }

    clog << "File name: (use a hyphen ('-') to display on screen) ";
    string fname;
    cin >> fname;

    ostream *pos = &cout;
    if (fname != "-")
    {
        pos = new ofstream(fname.c_str());
        isFileInHeap = true;
    }
    return pos;
}

int main()
{
    clog << "This program print (or save in file) the spiral-representation of matrix N x N\n"
         << "Enter N (odd): ";
    int n = 0;
    cin >> n;

    ostream *pos = ouputStreamManage();
    int **matrix = createMatrix(n);

    cout << endl;
    printMatrix(matrix, n);

    SpiralMatrix sp(matrix, n);
    // In the line above the pointer to the matrix is only remembed in the SpiralMatrix object.
    // In the line below the matrix will be converted to a linear representation which will be saved
    *pos << sp << endl;

    clog << "Set matrix[0][0] = 999" << endl;
    matrix[0][0] = 999;
    // In the next line only saved the linear representation will be printed.
    clog << sp << endl;
    clog << "Flush()" << endl;
    sp.flush();
    // In the line above the linear representation has been deleted and in the next line it will be recalculated
    clog << sp << endl;

    eraseMatrix(matrix, n);
    ouputStreamManage(pos);

    return 0;
}
