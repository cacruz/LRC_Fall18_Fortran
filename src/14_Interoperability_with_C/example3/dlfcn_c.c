#include <stdio.h>
#include <dlfcn.h>
main() {
	   printf("RTLD_LAZY=%d, RTLD_NOW=%d, RTLD_GLOBAL=%d, RTLD_LOCAL=%d\n",\
			         (int)RTLD_LAZY, (int)RTLD_NOW, (int)RTLD_GLOBAL, (int)RTLD_LOCAL);
}
