#include <iostream>
#include "linkedlist.h"
#include "arraylist.h"

using namespace std;

int main()
{
    List *list = new LinkedList();
    List *list2 = new ArrayList();
    list->insert(0, 10);
    list->remove(0);
    list2->insert(0, 20);
    cout << "length: " << list->length() << " - " << list2->length() << endl;

    delete list;
    delete list2;
    return 0;
}
