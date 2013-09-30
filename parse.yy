%{
#include <stdio.h>
#include <stdint.h>

#include "ast.hh"

extern "C" int yylex(void);
extern void yyerror(char const *);
%}

%define api.value.type {union semval}

%token NUMBER ID STRING IF ELSE SET 

%type <funcdef> program funcdef
%type <exprs> exprs block
%type <expr> expr
%type <params> params 
%type <podint> NUMBER
%type <podstr> ID STRING

%left '='
%left '+' '-'
%left '*' '/'
%precedence '['
%precedence SET
%precedence '!'

%%
program : exprs { Program = new FuncDef(new Parameters, $1); }
	;

exprs : expr { $$ = new Block; $$->exprs.push_back($1); }
      | exprs expr { $1->exprs.push_back($2); }
      ;

expr : '(' expr ')' { $$ = $2; }
     | ID { $$ = (Expr*) new Ident($1); }
     | NUMBER { $$ = (Expr*) new Number($1); }
     | STRING { $$ = (Expr*) new String($1); }
     | '!' expr { $$ = (Expr*) new UnaryOp('!', $2); }
     | expr '+' expr { $$ = (Expr*) new BinaryOp('+', $1, $3); }
     | expr '-' expr { $$ = (Expr*) new BinaryOp('-', $1, $3); }
     | expr '*' expr { $$ = (Expr*) new BinaryOp('*', $1, $3); }
     | expr '/' expr { $$ = (Expr*) new BinaryOp('/', $1, $3); } 
     | expr '=' expr { $$ = (Expr*) new BinaryOp('=', $1, $3); }
     | ID SET expr { $$ = (Expr*) new Assignment($1, $3); }
     | expr '[' exprs ']' { $$ = (Expr*) new FuncCall($1, $3); }
     | IF '(' expr ')' block ELSE block { $$ = (Expr*) new IfElse($3, $5, $7); }
     | funcdef { $$ = (Expr*) $1; }
     ;

block : '{' exprs '}' { $$ = $2; }
      ;

funcdef : '<' '>' '=' block { $$ = new FuncDef(new Parameters, $4); }
        | '<' params '>' '=' block { $$ = new FuncDef($2, $5); } 
        ;

params : ID { $$ = new Parameters; $$->params.push_back($1); }
       | params ID { $1->params.push_back($2); }
       ;
