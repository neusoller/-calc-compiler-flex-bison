#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>




#ifndef VARIABLE_TYPE
#define VARIABLE_TYPE
typedef enum
{
    INTEGER,
    FLOAT,
    STRING,
    BOOLEAN,
    UNDEFINED
} varType;


typedef struct variable_s
{
    char * name;
    union {
        int ival;
        float fval;
        char * sval;
        bool bval;
    } value;
    varType type;
} variable;

#endif