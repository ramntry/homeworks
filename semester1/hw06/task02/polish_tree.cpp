#include "polish_tree.h"
#include <sstream>        // Здесь можно найти строковой поток - похожий на файловый или стандартный, но работающий
#include <stdexcept>      // ... не с консолью или файлами - а со строками (оперативной памятью, иначе говоря) - еще
                          // ... один пример полиморфизма

TreeNode *OperationNode::getTreeNode(std::istream &in)
/* Используется в конструкторе OperationNode - см. ниже */
{
    char sym = '\0';
    in >> sym;                                 // Для похожих целей есть замечательный метод peek в istream, но он
    in.putback(sym);                           // ... не пропускает пробелы :(

    TreeNode *tmp = 0;                         // Указатель, во власти которого вся иерархия объектов
    if (sym == '(')                            // Открывающая скобочка говорит нам, что следом - операция и нужно
        tmp = new OperationNode(in);           // ... создавать поддерево с операцией
    else
        tmp = new LeafNode(in);                // ... или лист с константой, раз это не так

    if (in)
        return tmp;  // <stdexcept> именно за этим - библиотечные исключения достаточно удобны и универсальны
    throw std::invalid_argument("TreeCreateTimeError: Invalid argument - must be integer");
}

OperationNode::OperationNode(std::istream &in)
{
    char bracket = '\0';
    in >> bracket;                             // Пропуск бесполезной совершенно открывающей скобки

    in >> m_operation;                         // Сохраняем операцию и создаем правое и левое поддеревья -
    m_left = getTreeNode(in);                  // ... окажутся они лишь листьями или именно поддеревьями уже не наша
    m_right = getTreeNode(in);                 // ... забота. Алгоритм рекурсивен и работает на стеке вызовов функций
                                               // ... так же легко, как работал бы, скажем, на std::stack<T> от STL
    in >> bracket;
}

OperationNode::~OperationNode()
{
    delete m_left;   // Важный момент, если вам не нужны утечки памяти или проблемы при ее освобождении. То, что
    delete m_right;  // ... delete применим (это точно указатели, созданные new) гарантируется тем, что создаются они
                     // ... внутри конструктора класса, а не передаются в тот же конструктор извне. То, что этот
                     // ... delete, будучи примененным к листу ничего лишнего не сделает - тем, что лист не переопре-
                     // ... делял деструктор базового класса - а он пуст. То, что delete, примененный к поддереву с
                     // ... операцией вызовет рекурсивно такой же деструктор для его детей, то есть, вызовет именно
                     // ... ~OperationNode(), обеспечивается одновременным соблюдением двух вещей - существованием
                     // ... виртуального деструктора с реализацией у базового класса (пусть и пустой) и переопределе-
                     // ... нием его в OperationNode. Ясно, что здесь вызывается деструктор того типа, к которому
                     // ... принадлежат указатели - а это TreeNode с абсолютно ничего не делающим деструктором. Но он
                     // ... сцеплен (см. polish_tree.h) с деструктором поддерева с операцией.
}

int OperationNode::calculate() const
/* Если попросить поддерево посчитаться, то все, что ему нужно сделать - это в свою очередь попросить посчитаться
   своих сыновей (а каждый из них - будь это лист или поддерево - сделает это по-своему (полиморфизм), да только
   просить их надо одинаково и ответ они дают одинаково) и применить свою операцию к полученным ответам - и, конечно,
   сообщить результат нам. */
{
    switch (m_operation)
    {                                                        // ООП в своей красе - в самом ядре программы и
    case '+':                                                // ... комментировать нечего - все и так ясно.
        return m_left->calculate() + m_right->calculate();
    case '-':
        return m_left->calculate() - m_right->calculate();
    case '*':
        return m_left->calculate() * m_right->calculate();
    case '/':
        return m_left->calculate() / m_right->calculate();
    case '%':
        return m_left->calculate() % m_right->calculate();
    case '=':
        return m_left->calculate() == m_right->calculate();
    default:
        throw std::invalid_argument("TreeCalculateTimeError: Invalid operation symbol: must be +, -, *, /, % or =");
    }
}

void OperationNode::print(std::ostream &out) const
{
    out << " (" << m_operation;
    m_left->print(out);
    m_right->print(out);
    out << ')';
}

std::ostream &operator <<(std::ostream &out, const OperationNode &self)
/* Учим наше дерево печататься по запросу типа cout << polishTree; */
{
    std::stringstream sout;
    self.print(sout);  // Коли мы умеем печаться только в поток - давайте подсунем наивному print поток в память
    out << sout.str(); // ... и потом из нее уже выпишем все накопленное в наш cout. Или ofstream... Смотря что
                       // ... подсунет нам пользователь класса

    return out;
}
