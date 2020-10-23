#ifdef MCC
#else
#include <stdio.h>
#include <stdlib.h>
#endif

int main (int argc, char **argv)
{
  int a;
  a = 42*5;
  printf("Hello world %d\n", a);
    exit (0);
}
