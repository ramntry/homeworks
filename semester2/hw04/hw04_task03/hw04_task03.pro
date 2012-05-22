#-------------------------------------------------
#
# Project created by QtCreator 2012-03-04T13:47:29
#
#-------------------------------------------------

QT       += core gui

TARGET = hw04_task03
TEMPLATE = app


SOURCES += main.cpp\
        ubercalculator.cpp \
    stackmachine/stackmachine.cpp \
    stackmachine/oper.cpp \
    stackmachine/polishmachine/polishmachine.cpp

HEADERS  += ubercalculator.h \
    stackmachine/stackmachine.h \
    stackmachine/stretchablestack.h \
    stackmachine/stack.h \
    stackmachine/oper.h \
    stackmachine/polishmachine/polishmachine.h
