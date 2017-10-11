#include <stdlib.h>
#include <stdio.h>

/* te
	st */
//test td
char x;
char *y;
void main ()
{
	x = '\n';
	x = '\t';
	x = '\\';
	x = '\'';
	x = '\"';
	x = 'c';
	x = '0';
	x = '\x5D';
	y = "\'test1\'\t... Success !\n\"test2\"\t\\\x6F\x2f";
}