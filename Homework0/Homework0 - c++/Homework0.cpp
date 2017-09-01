/*
	CSC345 - Homework 0 - C++
	By: Brent Turner
*/
#include "stdafx.h"
#include <iostream>
using namespace std;

//Pass by Reference given (prototypes)
int square(int &x);
int twice(int &x);
int once(int &x);

int main()
{
	int x = 3;
	int y = square(x) + twice(x) + once(x); //y == 45, not 18
	cout << y << endl; //prints 45 not 18
	_sleep(1000);
    return 0;
}

int square(int &x)
{
	x = x * x;
	return x;
}

int twice(int &x)
{
	x = x * 2;
	return x;
}

int once(int &x)
{
	return x;
}

