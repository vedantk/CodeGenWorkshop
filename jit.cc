#include <stdio.h>
#include <stdlib.h>

#include "ast.hh"
#include "parse.tab.hh"

#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"

using namespace std;
using namespace llvm;

extern "C" FILE* yyin;
extern "C" int yylex(void);

FuncDef* Program;
static Module* TheModule;
static Frame RootClosure(NULL);
static IRBuilder<> Builder(getGlobalContext());
static FunctionPassManager* TheFPM;
static Type* Int64Ty = Type::getInt64Ty(getGlobalContext());

/*
 * In order to compare or perform arithmetic on expressions, we will discard
 * some type information and lower them to 64-bit integers.
 */
static Value* lower(Expr* e, Frame* frame)
{
    return Builder.CreateIntCast(e->CodeGen(frame), Int64Ty, true, "lower");
}

Value* Ident::CodeGen(Frame* frame)
{
    if (!frame->slots.count(id)) {
        printf("[Ident::CodeGen] Unbound identifier\n"); exit(1);
    }
    return Builder.CreateLoad(frame->bindings[frame->slots.lookup(id)], id);
}

Value* Number::CodeGen(Frame* frame)
{
    (void) frame;
    return ConstantInt::get(getGlobalContext(), APInt(64, num, true));
}

Value* String::CodeGen(Frame* frame)
{
    (void) frame;
    return ConstantDataArray::getString(getGlobalContext(), 
                                        StringRef((const char*) str));
}

Value* Block::CodeGen(Frame* frame)
{
    for (size_t i=0; i < exprs.size() - 1; ++i) {
        exprs[i]->CodeGen(frame);
    }
    return exprs.back()->CodeGen(frame);
}

Frame::Frame(Frame* parent)
{}

Frame::~Frame()
{}

Value* FuncCall::CodeGen(Frame* frame)
{
    if (typeid(*func) != typeid(FuncDef)) {
        printf("[FuncCall::CodeGen] Callee is not a FuncDef\n"); exit(1);
    }
    puts("FuncCall");
    return NULL;
}

Value* FuncDef::CodeGen(Frame* frame)
{
    puts("FuncDef");
    return NULL;
}

Value* Assignment::CodeGen(Frame* frame)
{
    /* Find the reference to our mutable variable. */
    AllocaInst* alloc;
    if (frame->slots.count(id)) {
        alloc = frame->bindings[frame->slots.lookup(id)];
    } else {
        /* Stack-allocate a variable in the entry block if we need to. */
        Function* TheFunction = Builder.GetInsertBlock()->getParent();
        IRBuilder<> allocaBuilder(&TheFunction->getEntryBlock(),
                                TheFunction->getEntryBlock().begin());
        alloc = allocaBuilder.CreateAlloca(Int64Ty, 0, id);
        frame->slots[id] = frame->bindings.size();
        frame->bindings.push_back(alloc);
    }

    /* Store into the alloca. */
    Value* val = value->CodeGen(frame);
    Builder.CreateStore(val, alloc);
    return val;
}

Value* UnaryOp::CodeGen(Frame* frame)
{
    Value* V = lower(arg, frame);
    switch (op) {
        case '!': return Builder.CreateNot(V);
        default:
            printf("[UnaryOp::CodeGen] Unsupported operation\n"); exit(1);
    }
}

Value* BinaryOp::CodeGen(Frame* frame)
{
    Value *L = lower(lhs, frame), 
          *R = lower(rhs, frame);
    switch (op) {
        case '+': return Builder.CreateAdd(L, R, "add");
        case '-': return Builder.CreateSub(L, R, "sub");
        case '*': return Builder.CreateMul(L, R, "mul");
        case '/': return Builder.CreateSDiv(L, R, "div");
        case '=': return Builder.CreateICmpEQ(L, R, "eq");
        default: 
            printf("[BinaryOp::CodeGen] Unsupported operation\n"); exit(1);
    }
}

Value* IfElse::CodeGen(Frame* frame)
{
    /* Compare the conditional test expression to 0. */
    Value* cond = Builder.CreateICmpNE(lower(test, frame),
            ConstantInt::get(getGlobalContext(), APInt(64, 0, true)),
            "ifcond");

    /* Generate blocks for the consequent, alternate, and phi branches. */
    Function* TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock* ConseqBB = BasicBlock::Create(getGlobalContext(), "conseq",
                                              TheFunction);
    BasicBlock* AlternBB = BasicBlock::Create(getGlobalContext(), "altern");
    BasicBlock* PhiBB = BasicBlock::Create(getGlobalContext(), "ifdone");
    Builder.CreateCondBr(cond, ConseqBB, AlternBB);

    /* Generate the consequent branch and branch to the phi node when done. */
    Builder.SetInsertPoint(ConseqBB);
    Value* conseq = consequent->CodeGen(frame);
    Builder.CreateBr(PhiBB);
    ConseqBB = Builder.GetInsertBlock();

    /* Generate the alternate branch and branch to the phi node when done. */
    TheFunction->getBasicBlockList().push_back(AlternBB);
    Builder.SetInsertPoint(AlternBB);
    Value* altern = alternate->CodeGen(frame);
    Builder.CreateBr(PhiBB);
    AlternBB = Builder.GetInsertBlock();

    /* Wire our parallel branches into the phi node. */
    TheFunction->getBasicBlockList().push_back(PhiBB);
    Builder.SetInsertPoint(PhiBB);
    PHINode* PN = Builder.CreatePHI(Int64Ty, 2, "ifphi");
    PN->addIncoming(conseq, ConseqBB);
    PN->addIncoming(altern, AlternBB);
    return PN;
}

void yyerror(char const* arg)
{
    printf("yyerror: %s\n", arg);
}

int main()
{
    yyin = stdin;
    yyparse();
    Program->CodeGen(&RootClosure);
    delete Program;
    return 0;
}
