#include "functions.h"
#include <math.h>

#define EPSILON 0.0001

extern int yyparse();
extern FILE *yyin;
extern FILE *yyout;
extern int yylineno;



char* valueToString(variable v) {
    char* str = (char*)malloc(sizeof(char)*10+1);
    
    switch(v.type) {
        case INTEGER:
            sprintf(str, "%d", v.value.ival);
            break;
        case FLOAT:
            sprintf(str, "%f", v.value.fval);
            break;
        case STRING:
            str = v.value.sval;
            break;
        case BOOLEAN:
            str = v.value.bval ? "true" : "false";
            break;
        default:
            str = "undefined";
    }
    
    return str;
}

char* typeToString(variable v) {
    char* str = (char*)malloc(sizeof(char)*10+1);
    
    switch(v.type) {
        case INTEGER:
            str = "INTEGER";
            break;
        case FLOAT:
            str = "FLOAT";
            break;
        case STRING:
            str = "STRING";
            break;
        case BOOLEAN:
            str = "BOOLEAN";
            break;
        default:
            str = "UNDEFINED";
    }
    
    return str;
}

variable trigonometricCalc(variable opv, variable v1) {
    char * op = opv.value.sval;
    variable result = {.type = FLOAT};
    float num1;

    if(v1.type == STRING)
    {
        result.type = UNDEFINED;
        result.value.sval = "SEMANTIC ERROR: Invalid operation for string type.\n";
        return result;
    } else if(v1.type == FLOAT) { 
    	num1 = v1.value.fval;
    } else {
    	num1 = (float)v1.value.ival; 
    }

    if (strcmp(op, "sin") == 0) result.value.fval = sinf(num1);
    else if (strcmp(op, "cos") == 0) result.value.fval = cosf(num1);
    else if (strcmp(op, "tan") == 0) result.value.fval = tanf(num1);
    else {
            result.type = UNDEFINED;
            result.value.sval= "SEMANTIC ERROR: Invalid trigonometric function.\n";
            return result;
    }

    return result;
}

variable arithmeticCalc(variable v1, variable opv, variable v2) {
       
    char * op = opv.value.sval;
    variable result = {.type = UNDEFINED};
    
    if (v1.type == INTEGER && v2.type == INTEGER) { // ENTER
        result.type = INTEGER;
        if (strcmp(op, "+") == 0) {
            result.value.ival = v1.value.ival + v2.value.ival;
        } else if (strcmp(op, "-") == 0) {
            result.value.ival = v1.value.ival - v2.value.ival;
        } else if (strcmp(op, "*") == 0) {
            result.value.ival = v1.value.ival * v2.value.ival;
        } else if (strcmp(op, "/") == 0) {
            if(v2.value.ival==0)
            {
                result.type = UNDEFINED;
                result.value.sval = "SEMANTIC ERROR: Division by zero\n";
                return result;
            }
            result.value.ival = v1.value.ival / v2.value.ival;
        } else if (strcmp(op, "**") == 0) {
        	result.value.ival = (int)pow(v1.value.ival, v2.value.ival);
        } else if (strcmp(op, "%") == 0) {
            result.value.ival = v1.value.ival % v2.value.ival;
        }
    } else if ((v1.type == INTEGER || v1.type == FLOAT) && (v2.type == INTEGER || v2.type == FLOAT)) { // ENTER I REAL
        result.type = FLOAT;
        float v1_f, v2_f;
        if (v1.type == INTEGER) {
            v1_f = (float)v1.value.ival;
        } else {
            v1_f = v1.value.fval;
        }
        if (v2.type == INTEGER) {
            v2_f = (float)v2.value.ival;
        } else  {
			v2_f = v2.value.fval;
		}
		if (strcmp(op, "+") == 0) {
			result.value.fval = v1_f + v2_f;
		} else if (strcmp(op, "-") == 0) {
			result.value.fval = v1_f - v2_f;
		} else if (strcmp(op, "*") == 0) {
			result.value.fval = v1_f * v2_f;
		} else if (strcmp(op, "/") == 0) {
			if(v2_f==0)
			{
				result.type = UNDEFINED;
				result.value.sval = "SEMANTIC ERROR: Division by zero.\n";
				return result;
			}
			result.value.fval = v1_f / v2_f;
		} else if (strcmp(op, "**") == 0) {
			if (v2_f == 0 && v1_f == 0) {
				result.type = UNDEFINED;
				result.value.sval = "SEMANTIC ERROR: 0**0 is undefined.\n";
				return result;
		    	}
		    	result.value.fval = pow(v1_f, v2_f);
		} else if (strcmp(op, "%") == 0) {
			result.type = UNDEFINED;
		    	result.value.sval = "SEMANTIC ERROR: Invalid operation for float type.\n";
		    	return result;
		} 
	} else if(v1.type == STRING || v2.type == STRING) { // CARÀCTER
        if (strcmp(op, "+") != 0) {
            result.type = UNDEFINED;
            result.value.sval="SEMANTIC ERROR: Invalid operation for string type.\n";
            return result;
        }
        result.type = STRING;
        char* v1_str = valueToString(v1);
        char* v2_str = valueToString(v2);
        int len = strlen(v1_str) + strlen(v2_str) + 1;
        result.value.sval = (char*)malloc(sizeof(char)*len+1);
        strcpy(result.value.sval, v1_str);
        strcat(result.value.sval, v2_str);
        
    } else if (v1.type == STRING && (v2.type == INTEGER || v2.type == FLOAT || v2.type == BOOLEAN)) { // CARÀCTER, ENTER, REAL I BOOLEÀ
        char num_str[32];
        if (v2.type == INTEGER) {
            snprintf(num_str, sizeof(num_str), "%d", v2.value.ival);
        } else if (v2.type == FLOAT){
            snprintf(num_str, sizeof(num_str), "%f", v2.value.fval);
        } else {
            snprintf(num_str, sizeof(num_str), "%i", v2.value.bval);
        }
        result.type = STRING;
        result.value.sval = (char*)malloc(sizeof(char) * (strlen(v1.value.sval) + strlen(num_str) + 1));
        
    } else if (v2.type == STRING && (v1.type == INTEGER || v1.type == FLOAT || v1.type == BOOLEAN)) {
    
        char num_str[32];
        if (v1.type == INTEGER) {
            snprintf(num_str, sizeof(num_str), "%d", v1.value.ival);
        } else if (v1.type == FLOAT) {
            snprintf(num_str, sizeof(num_str), "%f", v1.value.fval);
        } else {
            snprintf(num_str, sizeof(num_str), "%i", v1.value.bval);
        }
        result.type = STRING;
        result.value.sval = (char*)malloc(sizeof(char) * (strlen(num_str) + strlen(v2.value.sval) + 1));
    } else {
		result.type = UNDEFINED;
        result.value.sval="SEMANTIC ERROR: Invalid type for arithmetic operation.\n";
        return result;
	}
	
	return result;
}

variable booleanCalc(variable v1, variable opv, variable v2) {
    variable result;
    result.type = BOOLEAN;

    char * op = opv.value.sval;

    float v1_f, v2_f;
     if (v1.type == INTEGER) {
        v1_f = (float)v1.value.ival;
    } else if (v1.type == FLOAT) {
        v1_f = v1.value.fval;
    } else if (v1.type == BOOLEAN) {
        v1_f = v1.value.bval;
    }
    if (v2.type == INTEGER) {
        v2_f = (float)v2.value.ival;
    } else if (v2.type == FLOAT) {
        v2_f = v2.value.fval;
    } else if (v2.type == BOOLEAN) {
        v2_f = v2.value.bval;
    } else {
    	result.type = UNDEFINED;
        result.value.sval="SEMANTIC ERROR: Invalid type for boolean operation.\n";
        return result;
    }

    if (strcmp(op, "==") == 0) {
        result.value.bval = v1_f == v2_f;
    } else if (strcmp(op, ">") == 0) {
        result.value.bval = v1_f > v2_f;
    } else if (strcmp(op, ">=") == 0) {
        result.value.bval = v1_f >= v2_f;
    } else if (strcmp(op, "<") == 0) {
        result.value.bval = v1_f < v2_f;
    } else if (strcmp(op, "<=") == 0) {
        result.value.bval = v1_f <= v2_f;
    } else if (strcmp(op, "<>") == 0) {
        result.value.bval = v1_f != v2_f;
    }
    return result;
}

variable lenCalc(variable v) {
    variable result = {.type = INTEGER};

    if (v.type != STRING) {
        result.type = UNDEFINED;
        result.value.sval = "SEMANTIC ERROR: LEN only applies to strings.\n";
        return result;
    }

    // Retallar caràcters no visibles al final
    char* str = v.value.sval;
    size_t len = strlen(str);

    while (len > 0 && (str[len - 1] == ' ' || str[len - 1] == '\n' || str[len - 1] == '\r')) {
        len--;
    }

    result.value.ival = len - 1;  // Assignar la longitud neta
    return result;
}


variable substrCalc(variable str, variable start, variable length) {
    variable result = {.type = STRING};

    // Validació de tipus
    if (str.type != STRING || start.type != INTEGER || length.type != INTEGER) {
        result.type = UNDEFINED;
        result.value.sval = "SEMANTIC ERROR: SUBSTR requires (string; int; int).\n";
        return result;
    }

    int str_len = strlen(str.value.sval);
    int start_pos = start.value.ival;
    int substr_len = length.value.ival;

    // Validació de rang
    if (start_pos < 0 || substr_len < 0 || start_pos >= str_len) {
        result.type = UNDEFINED;
        result.value.sval = "SEMANTIC ERROR: Invalid start or length for SUBSTR.\n";
        return result;
    }

    // Ajustar la longitud si excedeix els límits de la cadena
    if (start_pos + substr_len > str_len) {
        substr_len = str_len - start_pos;
    }

    // Crear la subcadena
    result.value.sval = (char*)malloc(sizeof(char) * (substr_len + 1));
    strncpy(result.value.sval, str.value.sval + start_pos, substr_len);
    result.value.sval[substr_len] = '\0';

    return result;
}

