#ifdef MCC
#else
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif

int main(int argc, int **argv) {
     int a;
     a=malloc(5*8);
     a[0]=1;
     a[1]=42;
     printf("%llu - %llu\n", a, a[0]);
     printf("%d !\n", ++(++a[0]));
     printf("%llu - %llu\n", a, a[0]);
     eggs;
     return (a[0]);
}
