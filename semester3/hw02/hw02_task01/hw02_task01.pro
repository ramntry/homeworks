TEMPLATE = app
TARGET = test
DESTDIR = build
OBJECTS_DIR = build/obj

CONFIG -= qt
CONFIG += thread
CONFIG += debug
QMAKE_CXXFLAGS += -std=c++0x

LIBS += -L../../../libs/gtest/build -lgtest
INCLUDEPATH += ../../../libs/gtest/include
INCLUDEPATH += ../../../libs/boost

HEADERS += \
    test/test.h \
    test/NetworkTest.h \
    test/PersonalComputerTest.h \
    src/Network.h \
    src/NetworkAddress.h \
    src/NetworkDevice.h \
    src/OS.h \
    src/PersonalComputer.h \
    src/Program.h \
    src/Virus.h \

SOURCES += \
    test/test.cpp \
    test/NetworkTest.cpp \
    test/PersonalComputerTest.cpp \
    src/Network.cpp \
    src/NetworkDevice.cpp \
    src/PersonalComputer.cpp \
    src/Program.cpp \
    src/Virus.cpp \
    main.cpp \


