#ifdef MCC
#define FILE char
// pipeau: un FILE est quelque chose de plus complique, normalement...
#define EOF (-1)
// ca, par contre, c'est la vraie valeur de EOF.

#else
#include <stdio.h>
#include <stdlib.h>
#endif

int main (int argc, char **argv)
{
     int i, c;

     for (i=1; i<argc; i++)
     {
	  FILE *f;
	  printf("%s\n",argv[i]);

	  f = fopen (argv[i], "r");
	  printf("Fichier ouvert\n");
	  /* c=fgetc(f); */
	  /* printf("fgetc a fonctionné : %c\n",c); */
	  /* while ((c = fgetc (f)) != (-1)) */
	       /* printf("%d\n", c); */
	  printf("%d", 1 != 2);
	  fclose (f);
     }
     /* fflush (stdout); */
     exit (0);
}
