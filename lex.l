%{
#include <stdio.h>
#include <stdlib.h>
#include "parse.tab.hh"

int yywrap(void)
{
	return 1;
}

void setlval(void* ptr)
{
	void** target = (void**) &yylval;
	*target = ptr;
}
%}

ID	[a-zA-Z][a-zA-Z0-9_]*
INT	-?[0-9]+
PRIM	[\+\-\*/=\[\]<>()!,{}]
NOISE	[ \n\t,;]

%%

"if"	{ return IF; }
"else"	{ return ELSE; }
":="	{ return SET; }

{ID}	{ setlval(strdup(yytext)); return ID; }
{INT}	{ setlval((void*) atol(yytext)); return NUMBER; }
{PRIM}	{ return yytext[0]; }
'[^']*'	{ setlval(strdup(yytext)); return STRING; }

{NOISE}	{ }

.	{ printf("[lex.l] Unexpected lexeme: %s\n", yytext); }

%%
