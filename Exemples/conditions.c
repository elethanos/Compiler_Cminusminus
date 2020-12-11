#ifdef MCC
#else
#include <stdio.h>
#include <stdlib.h>
#endif

int main (int argc, char **argv)
{
  int a;
  a = 19;
  printf("Your age is %llu.\n", a);
  if(a>=18) {
    printf("You are above majority.\n");
    if(a<21) {
      printf("...except if you live in US!\n");
    }
  } else {
    printf("You are below majority.\n");
  }
  exit (0);
}
