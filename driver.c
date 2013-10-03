#include <stdint.h>

extern int64_t* lambda(int64_t* closure);

int main()
{
	return (int) (int64_t) lambda((void*) 0);
}
