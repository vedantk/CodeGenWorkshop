%{
#include <stdio.h>
#include <stdlib.h>
#include "parse.tab.hh"

int yywrap(void) 
{
	return 1;
}
%}

ID	[a-zA-Z][a-zA-Z0-9_]*
INT	-?[0-9]+
PRIM	[\+\-\*/=\[\]<>()!,]

%%

"if"	{ return IF; }
"else"	{ return ELSE; }
":="	{ return SET; }
"int"	{ return INT; }
"str"	{ return STR; }

{ID}	{ yylval = yytext; return ID; }
{INT}	{ yylval = atoi(yytext); return NUMBER; }
{PRIM}	{ return yytext[0]; }
'[^']*'	{ yylval = yytext; return STRING; }

[ \n\t] { /* Discard whitespace. */ }
.	{ printf("Unexpected lexeme: %s\n", yytext); }

%%
