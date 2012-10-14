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
INCLUDEPATH += ../../../libs

HEADERS += \
    test/test.h \
    test/NetworkTest.h \
    test/PersonalComputerTest.h \
    test/ParserTest.h \
    src/Network.h \
    src/NetworkAddress.h \
    src/NetworkDevice.h \
    src/OS.h \
    src/PersonalComputer.h \
    src/Program.h \
    src/Virus.h \
    src/netconf/parser.h \

SOURCES += \
    test/test.cpp \
    test/NetworkTest.cpp \
    test/PersonalComputerTest.cpp \
    test/ParserTest.cpp \
    src/Network.cpp \
    src/NetworkDevice.cpp \
    src/PersonalComputer.cpp \
    src/Program.cpp \
    src/Virus.cpp \
    src/netconf/parser.cpp \
    main.cpp \

