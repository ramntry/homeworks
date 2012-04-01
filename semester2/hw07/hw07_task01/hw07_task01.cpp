#include <iostream>
#include <string>

using namespace std;

class DevideByZeroException {};
enum { Virtue, Evil };

class Base
{
public:
    Base(string const& name)
        : mName(name)
    { cout << mName << " is constructed (Base)" << endl; }

    ~Base()
    { cout << mName << " is destructed (Base)" << endl; }

protected:
    string mName;
};

class Derive : public Base
{
public:
    Derive(string const& name)
        : Base(name)
    { cout << mName << " is constructed (Derive)" << endl; }

    ~Derive()
    { cout << mName << " is destructed (Derive)" << endl; }
};

int someHereticalFuncion(int divident, int divisor) throw (DevideByZeroException)
{
    Base b("bbb");
    Derive d("ddd");

    cout << "The darkness deepens..." << endl;
    if (divisor == 0)
        throw DevideByZeroException();

    Base doomed("fey");

    return divident / divisor;
}

int main()
{
    try {
        int ignor = someHereticalFuncion(1, 0);

    } catch (DevideByZeroException const&)
    {
        cout << "Oooops! Something is not pleasing to God happened..." << endl;
        return Evil;
    }

    return Virtue;
}
