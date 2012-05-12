HEADERS += \
    stack/stack.h \
    stack/simplestack.h \
    stack/stretchablestack.h \
    stackmachine.h \
    oper.h \
    qttest/stack_qttest.h \
    qttest/stackmachine_qttest.h \
    stack/dynamicstack.h

SOURCES += \
    hw02_task02.cpp \
    stackmachine.cpp \
    oper.cpp \
    qttest/stack_qttest.cpp \
    qttest/stackmachine_qttest.cpp

CONFIG += qtestlib

#QMAKE_LFLAGS += -z muldefs
