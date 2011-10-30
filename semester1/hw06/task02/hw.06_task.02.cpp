#include <iostream>
#include <fstream>
#include <string>
#include <stdexcept>
#include "polish_tree.h"

using namespace std;

int main(void)
{
    clog << "Enter a file name with integral syntax tree: ";
    string fname;
    cin >> fname;
    ifstream in(fname.c_str());
    if (!in)
    {
        cerr << "File " << fname << " not found" << endl;
        return 1;  // 1 - это код ошибки. Куда он идет? После завершения программы наберите в консоли
                   // ... echo %ERRORLEVEL% в windows или echo $? в *nix - и все поймете.
    }

    try
    {
        OperationNode polishTree(in);  // Заменой in на cin мы тут же переведем нашу программу на работу с консолью.
        cout << "(=" << polishTree << ' ' << polishTree.calculate() << ')' << endl;  // Неплохо бы в качестве ответа
    }                                                                                // ... выдавать строку, верную
    catch (std::invalid_argument e)                                                  // ... с позиций нашего же кода
    {
        cerr << e.what() << endl;      // Исключения из стандартной библиотеки имеют замечательный метод what,
                                       // ... который позволяет вытащить строку, ранее переданную конструктору
                                       // ... этого исключения.
    }
    return 0;      // 0 - это все в полном порядке
}
