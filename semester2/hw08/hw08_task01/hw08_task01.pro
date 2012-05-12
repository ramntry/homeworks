HEADERS += \
    Bag.h \
    Bag_qttest.h

CONFIG += qtestlib

SOURCES += \
    hw08_task01.cpp \
    Bag_qttest.cpp \
    Bag.cpp

QMAKE_CXXFLAGS += --std=c++0x
QMAKE_LFLAGS += -z muldefs
