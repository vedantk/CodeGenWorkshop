%{
#include <stdio.h>
#include <stdint.h>

#include "ast.hh"

extern "C" int yylex(void);
extern void yyerror(char const *);
%}

%define api.value.type {union semval}

%token NUMBER ID STRING INT STR IF ELSE SET 

%type <exprs> program exprs block
%type <expr> expr
%type <type> semtype
%type <podint> primitive
%type <vardef> vardef
%type <params> params paramlist
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

expr :'(' expr ')' { $$ = $2; }
     | ID { $$ = (Expr*) new Ident($1); }
     | NUMBER { $$ = (Expr*) new Number($1); }
     | STRING { $$ = (Expr*) new String($1); }
     | '!' expr { $$ = (Expr*) new UnaryOp('!', $2); }
     | expr '+' expr { $$ = (Expr*) new BinaryOp('+', $1, $3); }
     | expr '-' expr { $$ = (Expr*) new BinaryOp('-', $1, $3); }
     | expr '*' expr { $$ = (Expr*) new BinaryOp('*', $1, $3); }
     | expr '/' expr { $$ = (Expr*) new BinaryOp('/', $1, $3); } 
     | expr '=' expr { $$ = (Expr*) new BinaryOp('=', $1, $3); }
     | vardef SET expr { $$ = (Expr*) new Assignment($1, $3); }
     | expr '[' exprs ']' { $$ = (Expr*) new FuncCall($1, $3); }
     | '(' params ')' block { $$ = (Expr*) new FuncDef($2, $4); }
     | IF '(' expr ')' block ELSE block { $$ = (Expr*) new IfElse($3, $5, $7); }
     ;

vardef : ID '<' semtype '>' { $$ = new VarDef($1, $3); }
       ;

block : '{' exprs '}' { $$ = $2; }
      ;

params : %empty { $$ = new Parameters; }
       | paramlist { $$ = $1; }
       ;

paramlist : vardef { $$ = new Parameters; $$->params.push_back($1); }
	  | paramlist ',' vardef { $1->params.push_back($3); }
	  ;

primitive : INT { $$ = 0; }
	  | STR { $$ = 1; }
	  ;

semtype : primitive { $$.func = 0; $$.slots = $1; }
	| semtype ',' primitive { $1.func = 1; $1.slots |= $3 << ($1.arity++); }
	;
