%{
   #include<stdlib.h>
   #include<string.h>
   #include "y.tab.h"
%}

%%

[0-9]+			{ 
			  yylval.val=atoi(yytext);
			  return INTEGER;
			}		

"decl"			{return DECL;}
"enddecl"		{return ENDDECL;}
"integer"		{return INT;}
"boolean"		{return BOOL;}
"main"			{return MAIN;}
"begin"			{return BEG;}
"if"			{return IF;}
"then"			{return THEN;}
"else"			{return ELSE;}
"endif"			{return ENDIF;}
"while"			{return WHILE;}
"do"			{return DO;}
"endwhile"		{return ENDWHILE;}
"write"			{return WRITE;}
"read"			{return READ;}
"end"			{return END;}
"or"			{return OR;}
"and"			{return AND;}
"not"			{return NOT;}
"=="			{return EQ;}
">="			{return GE;}
"<="			{return LE;}
"!="			{return NE;}

[-+*/,();\[\]=><{}]		{return *yytext;}	

[a-zA-Z]+		{ 
			  yylval.var= malloc(20);
			  strcpy(yylval.var, yytext);
			  
			  return VARIABLE;
			}
[ \t\n]+			{}

.			{ 	yyerror("Invalid Character\n");}
%%

int yywrap(void)
{
   return 1;
}

