%{

#include "y.tab.h"
# include <stdio.h>
void yyerror(char *);
%}

%%

[ \t] ;

[0-9]+\.[0-9]+ {
		yylval=atof(yytext);
		return FLOAT;
	}

[0-9]+ {
	 	yylval=atoi(yytext);
		return INTEGER;
	}

\"[^a-z\n]*\" return STRING;


[-+*/\^()=<>\n\!#\$] return *yytext;

[,;] return DELIM;

NOT return NOT;

AND return AND;

OR return OR;

XOR return XOR;

DATA return DATA;

DEF return DEF;

FN[A-Z] {
		yylval=yytext[2];
		return FN;

	} 

DIM return DIM;

END return END;

FOR return FOR;

TO return TO;

STEP return STEP;

NEXT return NEXT;

GOSUB return GOSUB;

GOTO return GOTO;

IF return IF;

THEN return THEN;

LET return LET;

INPUT return INPUT;

PRINT return PRINT;

REM[^a-z\n]* return REM;

RETURN return RETURN;

STOP return STOP;

[A-Z] {
	 	yylval=*yytext;
		return LETTER;
	}

. { 
		printf("# invalid character ( %c ) detected\n", *yytext);
	 	yyerror("# scanner error\n");
	}


%%

int yywrap(void) {
	return 1;
}


