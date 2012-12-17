CONFIG -= qt
CONFIG += thread
CONFIG += release
QMAKE_CXXFLAGS += -std=c++0x

OBJECTS_DIR = obj

LIBS += -L../../../libs/gtest/build -lgtest
INCLUDEPATH += ../../../libs/gtest/include
INCLUDEPATH += ../../../libs/boost
INCLUDEPATH += ../../../libs

HEADERS += \
    BinarySearchTree/BinarySearchTree.h \
    BinarySearchTree/BinarySearchTreeTest.h \

SOURCES += \
    BinarySearchTree/BinarySearchTree.cpp \
    BinarySearchTree/BinarySearchTreeTest.cpp \
    main.cpp \

