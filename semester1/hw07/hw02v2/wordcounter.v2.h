/**
 * Структура и интерфейс узкоспециального бинарного дерева поиска, предназначенного для подсчета числа вхождений
 * строк во входной поток, организация которого остается за пользователем. Дерево имеет фиксированные полный размер
 * (в текущей реализации 1036-1040, в зависимости от разрядности платформы), полезный размер (в т.р. 1024) и
 * накладные расходы на хранение узла (в т. р. 7 байт вместе со счетчиком и терминирующим строку нулем + расходы
 * на выравнивание, при размере int = 4 в среднем - 2 байта.) При значении среднего размера слова 5-9 байт
 * предполагаемая емкость в т.р. - 85-64 слова (рекомендуется руководствоваться оценкой снизу, поскольку дерево
 * хранит уникальные строки, а общеизвестная статистика обыкновенно указывает частоты длин с учетом повторений)
 * Максимальное количество хранимых строк  - 128 (односимвольных в данном случае).
 * Дерево имеет монолитную структуру и при размещении новых элементов не использует внешних систем управления
 * памятью. Предусмотрено связывание деревьев в односвязный список.
 */

#pragma once

// Размер массивов. НЕ ДОЛЖЕН ПРЕВЫШАТЬ 256.
const unsigned int SIZE = 16;  // 256

// Размер заглушки в структруре бинарного дерева поиска WordBST от нулевого адреса до начала массива узлов WordNode.
// ... Составлен размером указателя на следующее дерево, индексом текущей позиции в дереве и счетчиком первого узла.
// ... Заглушка задана массивом int.
const unsigned int sizeDummy = (sizeof(int *) + 2 * sizeof(int)) / sizeof(int);

// Суммарный размер дополнительных (над счетчиком (значение) и строкой (ключ)) атрибутов узла дерева WordNode.
// ... Задан в байтах и необходим для организации выравнивания WordNode в теле WordBST (выравнивания требуют
// ... целочисленные счетчики. В данном случае атрибуты - индекс левого и правого сына дерева. В перспективе - цвет.
const unsigned int addFieldsSize = 2 * sizeof(unsigned char);

enum Direction { toLeftChild = 0, toRightChild, stopSearch };

// Ограничение на максимальное количество слов в дереве (128) позволяет хранить не указатели на узлы дерева, а их
// ... однобайтовые индексы в массиве.
union WordNode
{
    struct
    {
        unsigned char childs[2];   // Индексы левого и правого сына в массиве nodes структуры WordBST
        char firstLetter;          // Первый символ хранимой строки. Последующие храняться непрерывным массивом
                                   // ... в памяти. Следовательно, массив WordNode *nodes прерывен - его элементы
     // Счетчик в узел не включен  // ... расположены выровнено по int и с пропусками, соответсвующими "хвостам"
     // ... по описанным ниже      // ... хранимых в соседних слева структурах строк:
     // ... причинам
    };

int dummy;  // заглушка для выравнивания структуры по int
};

// В силу наличия вышеописанных пропусков в массиве узлов дерева очередная выровненная четверка (при sizeof(int) = 4)
// ... может трактоваться и как счетчик узла, и как заголовок самого узла (структура WordNode). Для обеспечения
// ... вариативного доступа используется объединение массивов счетчиков и узлов со смещением последнего на размер
// ... счетчика вправо (отсюда и ограничение 128 - счетчик и сам узел адресуются независимо, хотя и адрес узла всегда
// ... на 1 (по int) больше адреса счетчика - тогда как индексы соответсвующих счетчика и узла равны).
union WordBST
{
    struct
    {
        WordBST *next;               // Для связи деревьев в список
        int top;                     // Индекс текущей свободной позиции в конце массива.
        unsigned int counters[SIZE]; // Массив для доступа к счетчику i-ого узла дерева
    };
    struct
    {
int dummy[sizeDummy];  // Заглушка для организации необходимого смещения массивов счетчиков и узлов.

        WordNode nodes[SIZE];  //Массив для доступа к i-ому узлу дерева.
    };
};

/* [cn]lr i0 [cn]lr am0... [cn]lr programming0..
 *    |0     |1  |2   |3   |4  |5   |6  |7  |8  |9  |10 | - четверки байт. Индексы в массиве узлов
 * |0 |1     |2  |3   |4   |5  |6   |7  |8  |9  |10 |^    - четверки байт. Индексы в массиве счетчиков
 *                                               ^  (здесь: [cn] - счетчик, l - индекс левого сына, r - правого,
 *                                                          '.'- пропуски для выравнивания, 0 - терминирующие строки
 *                                                          нули, ^ - положение top)
 * В данном дереве расположены строки со следующими индексами:
 * 0 - "i", 2 - "am", 5 - "proramming". При построении сбалансированного бинарного дерева из них вышло бы следующее:
 * [cn]25 i0 [cn]00 am0... [cn]00 programming0..
 * (замечание: емкость определяется меньшим из индексов. В данном случае необходимый размер дерева - 9)
 */

int append(WordBST *tree, const char *word);  // Добавление строки в дерево без настройки индексов.
                                               // ... Возвращает число реально записанных байт без учета нуля.

/**
 * Осуществляет поиск строки в дереве. В случае успеха поиска инкрементирует счетчик узла, иначе
 * пытается добавить его в дерево с сохранением структуры последнего. В случае неудачи (недостаточно свободного места
 * в дереве) возвращает -1, иначе - старое значение счетчика строки (0, если было успешно добавлено новое).
 */
int handle(WordBST *tree, const char *word);

int printWordBST(WordBST *tree, int enumStarts = 1, unsigned int startsWith = 0);
