#include <stdio.h>
#include <stdlib.h>

#include "ast.hh"
#include "parse.tab.hh"

extern "C" FILE* yyin;
extern "C" int yylex(void);

Block* program;

llvm::Value* Block::CodeGen()
{
    puts("Block");
    return NULL;
}

llvm::Value* FuncCall::CodeGen()
{
    puts("Block");
    return NULL;
}

llvm::Value* FuncDef::CodeGen()
{
    puts("Block");
    return NULL;
}

llvm::Value* Assignment::CodeGen()
{
    puts("Assignment");
    return NULL;
}

llvm::Value* UnaryOp::CodeGen()
{
    puts("UnaryOp");
    return NULL;
}

llvm::Value* BinaryOp::CodeGen()
{
    puts("BinaryOp");
    return NULL;
}

llvm::Value* IfElse::CodeGen()
{
    puts("IfElse");
    return NULL;
}

llvm::Value* Number::CodeGen()
{
    puts("Number");
    return NULL;
}

llvm::Value* String::CodeGen()
{
    puts("String");
    return NULL;
}

void yyerror(char const* arg)
{
    printf("yyerror: %s\n", arg);
}

int main()
{
    yyin = stdin;
    yyparse();
    program->CodeGen();
}
