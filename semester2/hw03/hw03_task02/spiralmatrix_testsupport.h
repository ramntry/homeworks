#pragma once

int **createMatrix(int size);
void fillMatrix(int **matrix, int size, int mod = 0);
void printMatrix(int **matrix, int size, bool onTheSide = false);
void eraseMatrix(int **matrix, int size);
