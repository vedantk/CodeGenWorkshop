#include <stdio.h>
#include <stdlib.h>

#include "parse.tab.hh"

extern "C" FILE* yyin;
extern "C" int yylex(void);

void yyerror(char const* arg)
{
    printf("yyerror: %s\n", arg);
}

int main(int argc, char** argv)
{
    yyin = stdin;
    yyparse();
}
