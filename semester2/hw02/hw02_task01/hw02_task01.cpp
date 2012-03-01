#include <iostream>
#include "linkedlist.h"
#include "arraylist.h"

using namespace std;

int main()
{
    List *lists[2] = { new LinkedList(), new ArrayList(4) };

    for (int k = 0; k < 2; k++)
    {
        lists[k]->insert(0, 20);
        lists[k]->insert(0, 30);
        lists[k]->insert(0, 40);
        lists[k]->insert(1, 100);
        lists[k]->insert(4, 500);
        lists[k]->remove(2);
        lists[k]->remove(3);

        for (int i = 0; i < lists[k]->length(); i++)
            cout << lists[k]->at(i) << " ";
        cout << endl;

        cout << lists[k]->find(20) << " " << lists[k]->find(32) << endl;

        for (int i = 0; i < 32; i++)
            lists[k]->insert(i, i);

        for (int i = 0; i < 32; i++)
            cout << lists[k]->at(i) << " ";

        cout << endl << endl;

        delete lists[k];
    }

    return 0;
}
