#include <iostream>
#include <vector>
#include <iterator>
#include "karpa_rabina.h"

using namespace std;

int main(void)
{
    clog << "This program searches for a substring P in a string T" << endl;
    string text;
    clog << "T: ";
    getline(cin, text);

    string pattern;
    clog << "P: ";
    getline(cin, pattern);

    vector<int> offsets = karpaRabina(text.c_str(), pattern.c_str());

    clog << "Offsets: " << endl;
    copy(offsets.begin(), offsets.end(), ostream_iterator<int>(cout, " "));
    cout << endl;

    return 0;
}

