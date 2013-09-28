%{
#include <stdio.h>
#include <stdint.h>

#include <list>
using namespace std;

#include <gc/gc.h>

extern int yylex(void);
extern void yyerror(char const *);

/* Supported semantic types include i64 (0), char* (1), and up to 255-argument 
 * function with i64/char* arguments. */
typedef uint32_t sem_type;

struct var_decl {
	char* name;
	uint32_t type;
};

struct func_def {
	char* name;
	list<var_decl*> params;
};
%}

%union semval {
	void* ptr;
	char* str;
	int64_t i64;
	var_decl* var;
	func_def* func;
	list<semval*> exprs;
};

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
