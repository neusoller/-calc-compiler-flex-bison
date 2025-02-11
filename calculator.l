%{
	  #include <stdio.h>
	  #include <stdlib.h>
	  #include "structs.h"
	  #include "symtab.h"
	  extern FILE *yyin;
	  extern FILE* yyout;
	  #include "calculator.tab.h"
%}

%option noyywrap

int [0-9]*
float [0-9]*\.[0-9]*([eE][-+]?[0-9]+)?
string \".*\"

octal 0[oO][0-7]+
binari 0[bB][01]+
hexadecimal 0[xX][0-9a-fA-F]+

%%

 /* ENTER */
{int}           { yylval.var.type = INTEGER; yylval.var.value.ival = atoi(yytext); return INT; }

 /* REAL */
{float}         { yylval.var.type = FLOAT; yylval.var.value.fval = atof(yytext); return FL; }

 /* STRING */
{string}        { yylval.var.type = STRING; int len = yyleng-2; yylval.var.value.sval = (char*)malloc(sizeof(char)*len+1); /* Reservo la mida exacte per no malgastar memòria */
		  strncpy(yylval.var.value.sval, yytext+1, len); return STR; }

 /* OCTAL */
{octal}		{ yylval.var.type = INTEGER; yylval.var.value.ival = strtol(yytext + 2, NULL, 8); /* Base 8 */
		  return INT; }

 /* BINARI */
{binari}	{ yylval.var.type = INTEGER; yylval.var.value.ival = strtol(yytext + 2, NULL, 2); /* Base 2 */
		    return INT; }

 /* HEXADECIMAL */
{hexadecimal}	{ yylval.var.type = INTEGER; yylval.var.value.ival = strtol(yytext + 2, NULL, 16); /* Base 16 */
		  return INT; }

 /* BOOLEÀ */
"true"          { yylval.var.type = BOOLEAN; yylval.var.value.bval = 1; return BOOL; }
"false"         { yylval.var.type = BOOLEAN; yylval.var.value.bval = 0; return BOOL; }

 /* COMENTARI SIMPLE */
"//".*		 { yylval.var.type = STRING;
		   yylval.var.value.sval = strdup(yytext);
		   return SCOMMENT; }
		      
"#".*            { yylval.var.type = STRING;
		   yylval.var.value.sval = strdup(yytext);
		   return SCOMMENT; }

 /* COMENTARI MULTIPLE */
"/*"([^#=].|\n)*"*/"    { yylval.var.type = STRING;
			  yylval.var.value.sval = strdup(yytext); /* copio tot el comentari */
			  return MCOMMENT; }

 /* OPERADORS ARITMÈTICS */
"**"            { yylval.var.type = STRING; yylval.var.value.sval = "**"; return POW; }

"*"             { yylval.var.type = STRING; yylval.var.value.sval = "*"; return MUL; }
"/"             { yylval.var.type = STRING; yylval.var.value.sval = "/"; return DIV; }
"%"             { yylval.var.type = STRING; yylval.var.value.sval = "%"; return MOD; }

"+"             { yylval.var.type = STRING; yylval.var.value.sval = "+"; return ADD; }
"-"             { yylval.var.type = STRING; yylval.var.value.sval = "-"; return SUB; }

"("             { return LPAREN; }
")"             { return RPAREN; }

 /* OPERADORS RELACIONALS */
">"             { yylval.var.type = STRING; yylval.var.value.sval = ">"; return BOOLOP;  }
">="            { yylval.var.type = STRING; yylval.var.value.sval = ">="; return BOOLOP; }
"<"             { yylval.var.type = STRING; yylval.var.value.sval = "<"; return BOOLOP;  }
"<="            { yylval.var.type = STRING; yylval.var.value.sval = "<="; return BOOLOP; }
"="             { yylval.var.type = STRING; yylval.var.value.sval = "=="; return BOOLOP; }
"<>"            { yylval.var.type = STRING; yylval.var.value.sval = "<>"; return BOOLOP; }

 /* OPERADORS BOOLEANS */
"not"           { return NOT; }
"and"           { return AND; }
"or"            { return OR;  }

 /* ASSIGNACIONS */
":="            { return ASSIGN; }

 /* FUNCIONS TRIGONOMÈTRIQUES */
"sin"           { yylval.var.type = STRING; yylval.var.value.sval = "sin"; return SIN; }
"cos"           { yylval.var.type = STRING; yylval.var.value.sval = "cos"; return COS; }
"tan"           { yylval.var.type = STRING; yylval.var.value.sval = "tan"; return TAN; }

 /* LONGITUD DE CADENA */
"LEN"           { yylval.var.type = STRING; yylval.var.value.sval = "LEN"; return LEN; }
 /* EXTRACCIÓ DE SUBCADENA -> SUBSTRING */
"SUBSTR" 	{ yylval.var.type = STRING; yylval.var.value.sval = "SUBSTR"; return SUBSTR; } // no funciona
";"             { return ";";} // no funciona amb substring

 /* CONSTANTS PREDETERMINADES -> PI, E */
"pi"            { yylval.var.type = FLOAT; yylval.var.value.fval = 3.14159; return FL; } 
"e"             { yylval.var.type = FLOAT; yylval.var.value.fval = 2.71828; return FL; }

\n { return EOL; }

"\t"            {}
"\r"            {}
" "             {}

[a-zA-Z0-9]*   { yylval.var.name = (char*)malloc(sizeof(char) * yyleng + 1);
                 strncpy(yylval.var.name, yytext, yyleng);
                 variable aux;

                 if (sym_lookup(yytext, &aux) == SYMTAB_NOT_FOUND) { // tenim en consideració si hi ha nous IDs
                    sym_enter(yytext, &yylval.var);
                    return ID;
                 }
                 
                 return (aux.type == BOOLEAN) ? B_ID : A_ID;
               }

. {return LERR;}


<<EOF>>                 { return END; }

%%
