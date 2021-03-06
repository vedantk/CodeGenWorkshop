#pragma once

#include <vector>
#include <stdint.h>
#include <stdlib.h>

#include "llvm/IR/Value.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Function.h"

/*
 * Frames enable variable allocation, lookup, and capture.
 */
struct Frame
{
    llvm::StringMap<size_t> slots;
    std::vector<llvm::Value*> bindings;

    Frame() {}
    Frame(Frame* parent, llvm::Value* closure);

    llvm::Value* CaptureClosure(llvm::Function* F);
};

/*
 * The Expr interface ties AST construction and codegen together.
 */
struct Expr
{
    virtual ~Expr() {}
    virtual llvm::Value* CodeGen(Frame* frame) = 0;
    virtual void disp() = 0;
};

struct Ident : public Expr
{
    llvm::StringRef id;

    Ident(char* _str)
        : id(llvm::StringRef((const char*) _str))
    {}

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct Number : public Expr
{
    int64_t num;

    Number(int64_t n)
        : num(n)
    {}

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct String : public Expr
{
    char* str;

    String(char* _str)
        : str(_str)
    {}

    ~String() 
    {
        free(str);
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct Block : public Expr
{
    std::vector<Expr*> exprs;

    Block() {};

    ~Block()
    {
        for (Expr* e : exprs) {
            delete e;
        }
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct NativeCall : public Expr
{
    char* id;
    Block* args;

    NativeCall(char* _id, Block* _args)
        : id(_id), args(_args)
    {}

    ~NativeCall()
    {
        free(id);
        delete args;
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct FuncCall : public Expr 
{
    Expr* func;
    Block* args;

    FuncCall(Expr* _func, Block* _args)
        : func(_func), args(_args)
    {}

    ~FuncCall()
    {
        delete func;
        delete args;
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct Parameters
{
    std::vector<char*> params;

    ~Parameters()
    {
        for (char* i : params) {
            free(i);
        }
    }
};

struct FuncDef : public Expr 
{
    llvm::Function* F;
    Parameters* params;
    Block* block;

    FuncDef(Parameters* _params, Block* _block)
        : params(_params), block(_block)
    {}

    ~FuncDef()
    {
        delete params;
        delete block;
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct Assignment : public Expr
{
    llvm::StringRef id;
    Expr* value;

    Assignment(char* _id, Expr* _value)
        : id(llvm::StringRef((const char*) _id)), value(_value)
    {}

    ~Assignment()
    {
        free((void*) id.data());
        delete value;
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct UnaryOp : public Expr 
{
    char op;
    Expr* arg;

    UnaryOp(char _op, Expr* _arg)
        : op(_op), arg(_arg)
    {}

    ~UnaryOp()
    {
        delete arg;
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct BinaryOp : public Expr
{
    char op;
    Expr *lhs, *rhs;

    BinaryOp(char _op, Expr* _lhs, Expr* _rhs)
        : op(_op), lhs(_lhs), rhs(_rhs)
    {}

    ~BinaryOp()
    {
        delete lhs;
        delete rhs;
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

struct IfElse : public Expr
{
    Expr *test; 
    Block *consequent, *alternate;

    IfElse(Expr* _test, Block* _conseq, Block* _altern)
        : test(_test), consequent(_conseq), alternate(_altern)
    {}

    ~IfElse()
    {
        delete test;
        delete consequent;
        delete alternate;
    }

    virtual llvm::Value* CodeGen(Frame* frame);
    virtual void disp();
};

/*
 * The parser generator uses a type union to manage semantic values.
 */
union semval {
    Expr* expr;
    Block* exprs;
    Parameters* params;
    FuncDef* funcdef;
    int podint;
    char* podstr;
};

/*
 * We treat compilation units as functions in order to simplify codegen.
 */
extern FuncDef* Program;
