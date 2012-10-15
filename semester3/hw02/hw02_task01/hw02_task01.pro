TEMPLATE = app
TARGET = test
DESTDIR = build
OBJECTS_DIR = build/obj

CONFIG -= qt
CONFIG += thread
CONFIG += release
QMAKE_CXXFLAGS += -std=c++0x

LIBS += -L../../../libs/gtest/build -lgtest
INCLUDEPATH += ../../../libs/gtest/include
INCLUDEPATH += ../../../libs/boost
INCLUDEPATH += ../../../libs

HEADERS += \
    test/test.h \
    test/NetworkTest.h \
    test/PersonalComputerTest.h \
    test/NetconfParserTest.h \
    src/Network.h \
    src/NetworkAddress.h \
    src/NetworkDevice.h \
    src/OS.h \
    src/PersonalComputer.h \
    src/VirtualServer.h \
    src/Program.h \
    src/Virus.h \
    src/netconf/pc.h \
    src/netconf/grammar.h \
    src/netconf/NetconfParser.h \
    src/netconf/Constructor.h \

SOURCES += \
    test/test.cpp \
    test/NetworkTest.cpp \
    test/PersonalComputerTest.cpp \
    test/NetconfParserTest.cpp \
    src/Network.cpp \
    src/NetworkDevice.cpp \
    src/PersonalComputer.cpp \
    src/VirtualServer.cpp \
    src/Program.cpp \
    src/Virus.cpp \
    src/netconf/NetconfParser.cpp \
    src/netconf/Constructor.cpp \
    main.cpp \

