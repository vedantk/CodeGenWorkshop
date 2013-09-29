%{
#include <stdio.h>
#include <stdint.h>

#include "ast.hh"

extern "C" int yylex(void);
extern void yyerror(char const *);
%}

%define api.value.type {union semval}

%token IF ELSE SET NUMBER STRING ID INT STR
%type<int64_t> NUMBER
%type<char*> ID STRING

%left '='
%left '+' '-'
%left '*' '/'
%precedence '['
%precedence SET
%precedence '!'

%%

exprs : %empty
      | exprs expr
      ;

expr : atom
     | funcall
     | funcdef
     | '!' expr
     | '(' expr ')'
     | expr '+' expr
     | expr '-' expr
     | expr '*' expr
     | expr '/' expr
     | expr '=' expr
     | vardecl SET expr
     | IF '(' expr ')' block ELSE block
     ;

atom : NUMBER
     | STRING
     ;

funcall : expr '[' exprs ']'
	;

funcdef : '(' params ')' block
	;

vardecl : ID '<' semtype '>'
	;

block : '{' exprs '}'
      ;

params : %empty
       | paramlist
       ;

paramlist : vardecl
	  | paramlist ',' vardecl
	  ;

type : INT
     | STR
     ;

semtype : type
	| semtype ',' type
	;
