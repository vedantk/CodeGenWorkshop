#pragma once

#include <gc/gc.h>

#include <stdint.h>

#include <list>
using namespace std;

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

union semval {
	void* ptr;
	char* str;
	int64_t i64;
	var_decl* var;
	func_def* func;
	list<semval*>* exprs;
};
