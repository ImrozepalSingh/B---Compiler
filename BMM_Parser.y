%token FLOAT INTEGER STRING DELIM NOT AND OR XOR DATA DEF FN DIM END FOR TO STEP NEXT GOSUB GOTO IF THEN LET INPUT PRINT REM RETURN STOP LETTER Uminus
%left '+' '-'
%left '*' '/'
%left UMinus
%left '^'
%left '(' ')'

%{
int yylex(void);
void yyerror(char *);

#include <stdlib.h>
#include <stdio.h>

extern FILE * yyin;

int indexMax=0; //current max index while parsing
int fnTable[26]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; //table for declared functions
int isEnd=0;
int endCount=0;


/*** check index within range ***/

void range(int i){

	if(i<1||i>9999)
		printf("#parsing error: index %d out of range(1-9999)\n",i);

	}


%}

%%

program:
	stmts {
			if(endCount==0)
				printf("#parsing error:Missing end statement\n");

		}
	;

let_stmt:
	LET int_var '=' num_expr {

					if($2==-1||$4==-1) $$=-1;
				}	



	|LET string_var '=' STRING {

					if($2==-1) $$=-1;
				}	
	|LET string_var '=' string_var {

					if($2==-1||$4==-1) $$=-1;
				}

	|LET error '=' num_expr {printf("#type error\n"); $$=-1;}

	|LET string_var '=' error {printf("#type error\n"); $$=-1;}
	|LET int_var '=' error {printf("#type error\n"); $$=-1;}
	|LET error {$$=-1; printf("let stmt error\n");}
	;




for_loop:
	INTEGER for_line stmts INTEGER next_line {
						if($2!=$5)
							printf("(line%d)for loop match detected but var name different error\n",$1);
					}
	|INTEGER for_line stmts {printf("(line%d)unmatched for loop error",$1);}
	;

for_line:	
	FOR error '\n'{printf("for line error \n");}
	| FOR LETTER '=' num_expr TO num_expr STEP num_expr '\n'{$$=$2;}
	| FOR LETTER '=' num_expr TO num_expr '\n'{$$=$2;}
	;
	
next_line:
	NEXT LETTER '\n' {$$=$2;}
	| NEXT error '\n' {printf("next line error \n");}
	;

end_stmt:
	END{ isEnd=1; endCount++; }
	;

stop_stmt:
	STOP 
	;

rem_stmt:
	REM
	;
return_stmt:
	RETURN
	;


goto_stmt:
	GOTO INTEGER
	| GOTO error {$$=-1; printf("goto stmt target error\n");}
	;


gosub_stmt:
	GOSUB INTEGER
	| GOSUB error {$$=-1;printf("gosub stmt target error\n");}
	;

data_type:
	INTEGER
	| FLOAT
	| STRING
	;

data_stmt:
	DATA data_type
	|data_stmt DELIM data_type
	|DATA error{$$=-1; printf("data stmt error\n");}
	;

input_stmt:
	INPUT var {$$=$2;}
	|input_stmt DELIM var  {if($1==-1) $$=-1; else $$=$3;}
	|INPUT error {$$=-1;  printf("input stmt error\n");}
	;
stmts:
	stmt 
	| stmts stmt
	;

stmt:
	indexed_stmt 
	| for_loop
	|'\n'
	;
indexed_stmt:
	INTEGER sub_stmt {

					if(isEnd==0&&endCount>0)
							printf("#parsing error(line %d):statement after end statement\n",$1);


					if(isEnd){
						isEnd=0;
						if($1<=indexMax)
							printf("#parsing error(line %d):End statement does not have maximum index\n",$1);

					}
					$$=$1; 
					range($1);
					if($1>indexMax) indexMax=$1;	

					if($2==-1)
						printf("-----#parsing error(line %d):statement error [^^^ more details may be printed above ^^^]----- \n",$1);

				}
	|sub_stmt {
					$$=-1;
					printf("-----#parsing error:statement without index [^^^ more details may be printed above ^^^]----- \n");
				}
	;

sub_stmt:
	def_stmt '\n'
	| dim_stmt '\n'
	| stop_stmt '\n'
	| rem_stmt '\n'
	| data_stmt '\n'
	| input_stmt '\n'
	| goto_stmt '\n'
	| gosub_stmt '\n'
	| return_stmt '\n'
	| if_stmt '\n'
	| let_stmt '\n'
	| print_stmt '\n'
	| print_stmt DELIM '\n'
	| end_stmt '\n'
	| error '\n' {printf("unrecognized stmt type\n");}
	;

def_stmt:
	DEF FN '=' num_expr  {
					$$=0;
					if(fnTable[$2-'A']==0) fnTable[$2-'A']=1; 
					else {printf("#parsing error: fn%c def already exists\n"); $$=-1;}
				}
	| DEF FN '(' var ')' '=' num_expr {
					$$=0;
					if(fnTable[$2-'A']==0) fnTable[$2-'A']=1; 
					else {printf("#parsing error: fn%c def already exists\n",$2); $$=-1;}

		}
	|DEF error {$$=-1; printf("def stmt error\n");}
	;

dim_stmt:
	DIM brktDecl {
					$$=0;
					if($2==-1)
						$$=-1;

		}
	;

brktDecl:
	nf_var'('INTEGER')' {$$=$1;}
	|nf_var'('INTEGER DELIM INTEGER')' {$$=$1;}
	|nf_var'('INTEGER')' DELIM brktDecl {if($1==-1) $$=-1; else $$=$6;}
	|nf_var'('INTEGER DELIM INTEGER')' DELIM brktDecl {if($1==-1) $$=-1; else $$=$6;}
	|nf_var error {printf("#parsing error: DIM declaration error\n"); $$=-1;}
	|brktDecl error {printf("#parsing error: DIM declaration error\n"); $$=-1;}
	;



num_expr:
	INTEGER
	|FLOAT
	| FN
	| FN'(' num_expr ')'
	| int_var { $$=$1;}
	| '(' num_expr ')' {$$=$2;}
	| num_expr '+' num_expr {if($1==-1||$3==-1) $$=-1;}
	| num_expr '-' num_expr {if($1==-1||$3==-1) $$=-1;}
	
	| num_expr '*' num_expr {if($1==-1||$3==-1) $$=-1;}
	
	
	| num_expr '/' num_expr {if($1==-1||$3==-1) $$=-1;}
	
	| num_expr '^' num_expr {if($1==-1||$3==-1) $$=-1;}

	|'-' num_expr %prec Uminus {$$=$2;}

	|error {$$=-1; printf("num expression error\n");}
	;


rel_expr:
	num_expr '=' num_expr {if($1==-1||$3==-1) $$=-1;}
	|num_expr '<' '>' num_expr {if($1==-1||$4==-1) $$=-1;}
	|num_expr '<' num_expr {if($1==-1||$3==-1) $$=-1;}
	|num_expr '>' num_expr {if($1==-1||$3==-1) $$=-1;}
	|num_expr '<' '=' num_expr {if($1==-1||$4==-1) $$=-1;}
	|num_expr '<''=' num_expr {if($1==-1||$4==-1) $$=-1;}
	|num_expr '>''=' num_expr {if($1==-1||$4==-1) $$=-1;}
	|string_var'='string_var {if($1==-1||$3==-1) $$=-1;}
	|string_var'<''>'string_var {if($1==-1||$4==-1) $$=-1;}
	|string_var'='STRING {if($1==-1) $$=-1;}
	|string_var'<''>'STRING {if($1==-1) $$=-1;}
	|error {$$=-1; printf("rel expression error\n");}
	;

nr_expr:
	num_expr
	|rel_expr
	;

log_expr:
	nr_expr 
	|NOT nr_expr {$$=$2;}
	|nr_expr AND nr_expr {if($1==-1||$3==-1) $$=-1;}
	|nr_expr OR nr_expr {if($1==-1||$3==-1) $$=-1;}
	|nr_expr XOR nr_expr {if($1==-1||$3==-1) $$=-1;}
	 
	|NOT log_expr {$$=$2;} {if($1==-1||$3==-1) $$=-1;}
	|log_expr AND log_expr {if($1==-1||$3==-1) $$=-1;}
	|log_expr OR log_expr {if($1==-1||$3==-1) $$=-1;}
	|log_expr XOR log_expr {if($1==-1||$3==-1) $$=-1;}
	| error {$$=-1; printf("log expression error\n");}
	;

if_stmt:
	IF rel_expr THEN INTEGER {$$=$2;}
	|IF error {$$=-1; printf("if stmt error\n");}
	|IF num_expr THEN INTEGER {$$=$2; printf("num_expr in IF condition error\n"); }
	;

print_stmt:
	PRINT string_var  {$$=$2;}
	|PRINT STRING  {$$=$2;}
	|PRINT num_expr  {$$=$2;}
	|print_stmt DELIM string_var {

					if($1==-1||$3==-1) $$=-1;
				}
	|print_stmt DELIM num_expr  {

					if($1==-1||$3==-1) $$=-1;
				}
	|print_stmt DELIM  STRING  {

					if($1==-1) $$=-1;
				}
	|PRINT error {$$=-1; printf("print stmt error\n");}
	;


var:
	|int_var 
	|string_var 
	;


nf_var:
	LETTER
	|LETTER INTEGER {

			if($2>9||$2<0){printf("#parsing error: variable name %c%d is invalid\n",$1,$2); $$=-1;}

			}
	| error {$$=-1; printf("identifier error\n");}
	;


int_var:
	nf_var int_format {
				if($1==-1)
					$$=-1;
				else
					$$=$2;


			}
	|nf_var '(' INTEGER ')' {
				if($1==-1)
					$$=-1;
				else
					$$='%';

			}
	|nf_var '(' num_expr ')' {
				if($1==-1)
					$$=-1;
				else
					$$='%';

			}
	|nf_var '(' num_expr DELIM num_expr ')' {
				if($1==-1)
					$$=-1;
				else
					$$='%';

			}
	|nf_var '(' INTEGER DELIM INTEGER ')' {
				if($1==-1)
					$$=-1;
				else
					$$='%';
	}	
	|nf_var {
				if($1==-1)
					$$=-1;
				else
					$$='%';

			}
	|nf_var error {$$=-1; printf("format error\n");}
	;

string_var:
	nf_var string_format {
				if($1==-1)
					$$=-1;
				else
					$$=$2;


			}
	|nf_var error {$$=-1;printf("format error\n");}
	;

	

int_format:
	'#' {$$=$1;}
	| '%' {$$=$1;}
	| '!' {$$=$1;}
	;

string_format:
	'$' {$$=$1;}
	;






%%
void yyerror(char *s) {
	fprintf(stderr, "%s\n", s);
}

int main(){

	FILE *fp;
	fp=fopen("sample.txt","r");
	yyin=fp;
 
	yyparse();

	printf("---parsing end---");
}


