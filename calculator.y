%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include <stdbool.h>
	#include <math.h>

	int errflag = 0;

	extern FILE* yyin;
	extern int yylineno;

	extern int yylex();
	extern void yyerror(char *explanation);

	FILE* flog;

	int yyterminate()
	{
	  return 0;
	}
%}

%code requires {
  	#include "symtab.h"
	#include "functions.h"
}

%union { variable var; };

%token <var> FL INT BOOL STR B_ID ID A_ID ADD SUB MUL DIV MOD POW BOOLOP SIN COS TAN LEN SUBSTR SCOMMENT MCOMMENT
%token ASSIGN LPAREN RPAREN AND OR NOT EOL END LERR
%type <var> statement statement_list arithmetic_op1 arithmetic_op2 arithmetic_op3 boolean_op1 boolean_op2 boolean_op3 boolean_arithmetic exp arithmetic boolean trigonometric

%start program

%%
program : statement_list END;

statement_list : statement_list statement | statement;

statement:
		ID ASSIGN exp	{			if($3.type == UNDEFINED){
								yyerror($3.value.sval);
							} else {
								sym_enter($1.name, &$3);
								fprintf(flog, "\nLínia %d: -- ASSIGNACIÓ -- (%s := %s)\n", yylineno, $1.name, valueToString($3));
								fprintf(flog, "\t-> NOM: %s\n\t-> TIPUS: %s\n\t-> VALOR: %s\n", $1.name, typeToString($3), valueToString($3));
								
							}
							yylineno++;
				}
		| exp 		{			if($1.type == UNDEFINED){
								yyerror($1.value.sval);
							} else {
								if ($1.name == NULL) fprintf(flog, "\nLínia %d: L'expressió és nul·la. Valor: '%s'\n", yylineno, valueToString($1));
								else {
									fprintf(flog, "\nLínia %d: -- EXPRESSION -- %s serà %s\n", yylineno, $1.name, valueToString($1));
									fprintf(flog, "\t-> TIPUS: %s\n\t-> VALOR: %s\n", typeToString($1), valueToString($1));	
								}
							}
							yylineno++;
				}
		| EOL		{			yylineno++;	}
		| SCOMMENT	{			fprintf(flog, "Línia %d: Comentari simple '%s'\n", yylineno, $1.value.sval);
							free($1.value.sval);
							yylineno++; 
				}
		| MCOMMENT	{ 			fprintf(flog, "Línia %d: Comentari múltiple '%s'\n", yylineno, $1.value.sval);
							free($1.value.sval);
							yylineno++;
				}
		| END		{			yyerror("Final de fitxer. Execució completada !!\n"); 
							YYABORT;
				}
		| LERR EOL	{			yyerror("Error lèxic: caràcter invàlid.\n");
							yylineno++;
				}
		| LERR 		{			yyerror("LEXICAL ERROR: invalid character.\n");	
				}
		| error	EOL	{			if (errflag == 1) errflag = 0;
							else fprintf(flog,"\nLínia %d: SYNTAX ERROR: No hi ha cap regla que coicideixi.\n", yylineno);
							yylineno++;
				}
		;

exp: arithmetic | boolean;

trigonometric: SIN | COS | TAN;

 /* ---- nivell més baix ---- */
arithmetic:
		arithmetic_op1

		| arithmetic ADD arithmetic_op1		{ $$ = arithmeticCalc($1, $2, $3); }
		| arithmetic SUB arithmetic_op1 	{ $$ = arithmeticCalc($1, $2, $3); }
		| ADD arithmetic_op1			{ $$ = $1; }
		| SUB arithmetic_op2			{ $$.type = $2.type;

							  if ($2.type == INTEGER) $$.value.ival = -$2.value.ival;
							  else if ($2.type == FLOAT) $$.value.fval = -$2.value.fval;
							}
		;

 /* ---- nivell intermedi: operadors de mul, div i mod ---- */
arithmetic_op1:
		arithmetic_op2
		| arithmetic_op1 MUL arithmetic_op2 	{ $$ = arithmeticCalc($1, $2, $3); }
		| arithmetic_op1 DIV arithmetic_op2 	{ $$ = arithmeticCalc($1, $2, $3); }
		| arithmetic_op1 MOD arithmetic_op2	{ $$ = arithmeticCalc($1, $2, $3); }
		;

 /* ---- nivell més alt: operador pow ---- */
arithmetic_op2:
		arithmetic_op3 
		| arithmetic_op2 POW arithmetic_op3	{ $$ = arithmeticCalc($1, $2, $3); }
		;

 /*  ---- nivell base  ----*/
arithmetic_op3:

		LPAREN arithmetic RPAREN						{ $$ = $2; }
		| INT 									{ if($1.type == UNDEFINED) yyerror($1.value.sval);
											  else $$ = $1;
											}	
		| FL									{ if($1.type == UNDEFINED) yyerror($1.value.sval);
											  else $$ = $1;
											}
		| STR									{ if($1.type == UNDEFINED) yyerror($1.value.sval);
											  else $$ = $1;
											}
		| LEN LPAREN arithmetic RPAREN             				{ $$ = lenCalc($3); }
    		| SUBSTR LPAREN arithmetic ';' arithmetic ';' arithmetic RPAREN 	{ $$ = substrCalc($3, $5, $7); }
		| A_ID									{ if(sym_lookup($1.name, &$1) == SYMTAB_NOT_FOUND) {	
												yyerror("SEMANTIC ERROR: VARIABLE NOT FOUND.\n");
												errflag = 1;
												YYERROR;
											  } else { 
												$$.type = $1.type;
												$$.value=$1.value;
											  }
											}
		|ID									{ 	if(sym_lookup($1.name, &$1) == SYMTAB_NOT_FOUND) {	
													yyerror("SEMANTIC ERROR: VARIABLE NOT FOUND.\n");
													errflag = 1;
													YYERROR;
												} else { 
													$$.type = $1.type; 
													$$.value=$1.value;
												}
											}
		| trigonometric LPAREN arithmetic RPAREN				{ $$ = trigonometricCalc($1, $3); }
		;

boolean: boolean_op1 
		| boolean OR boolean_op1	{ $$.name = NULL;
						  $$.type = BOOLEAN;
						  $$.value.bval = $1.value.bval || $3.value.bval; }
		;

boolean_op1: boolean_op2 
		| boolean_op1 AND boolean_op2 	{ $$.name = NULL;
						  $$.type = BOOLEAN;
						  $$.value.bval = $1.value.bval && $3.value.bval; }
		;

boolean_op2: boolean_op3 
		| NOT boolean_op2 		{ $$.name = NULL;
						  $$.type = BOOLEAN;
						  $$.value.bval = !($2.value.bval); }
		;

boolean_op3: boolean_arithmetic
		| LPAREN boolean RPAREN		{ $$ = $2; } /* permet utilitzar parèntesis */
		| BOOL 	 			{ $$ = $1; } /* permet utilitzar valors com true i false */
		| B_ID				{ if(sym_lookup($1.name, &$1) == SYMTAB_NOT_FOUND) {
							yyerror("SEMANTIC ERROR: VARIABLE NOT FOUND\n");
							errflag = 1; YYERROR;
						  } else {
							$$.type = $1.type;
							$$.value=$1.value;
						  }
						}
		;

boolean_arithmetic: arithmetic BOOLOP arithmetic 	{ booleanCalc($1, $2, $3); }

%%

void yyerror(char *explanation) {
    /*if (strcmp(explanation, "End of the file, execution COMPLETED\n") == 0) {
    	printf("%s", explanation);
    } else{ 
    	printf("Line %d\t%s", yylineno, explanation);
    	fprintf(flog,"Line %d\t%s", yylineno, explanation);
    }*/
    
    if (strcmp(explanation, "End of the file, execution COMPLETED\n") != 0)
    	fprintf(flog,"Line %d\t%s", yylineno, explanation);
}

int main(int argc, char** argv) {
    flog = fopen("sortida_proves.txt", "w");
    
    if(flog == NULL){
        printf("Error: Unable to open log file log.txt\n");
        return 1;
    }

    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (yyin == NULL) {
            printf("Error: Unable to open file %s\n", argv[1]);
            return 1;
        }
    } else {
        printf("Error: No input file specified\n");
        return 1;
    }
    yyparse();
    
    if(fclose(flog) != 0){
        printf("Error: Unable to close log file log.txt\n");
        return 1;
    }

    return 0;
}

