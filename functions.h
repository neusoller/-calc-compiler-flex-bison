#ifndef FUNCTIONS_H
#define FUNCTIONS_H


#include "symtab.h"
#include "structs.h"
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>


char* valueToString(variable v);
char* typeToString(variable v);
variable arithmeticCalc(variable v1, variable opv, variable v2);
variable booleanCalc(variable v1, variable opv, variable v2);
variable trigonometricCalc(variable opv, variable v1);
void yyerror(char *explanation);
variable lenCalc(variable v);
variable substrCalc(variable str, variable start, variable length);



#endif
