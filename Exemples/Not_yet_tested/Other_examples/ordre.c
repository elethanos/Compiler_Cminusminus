/* int maf(int a, int b, int c, int d, int e, int f, int g, int h, int i) */
/* { */
/*      int coucou; */
/*      coucou = 42; */
/*      printf("%d %d %d %d %d %d %d %d %d %d", a,b,c,d,e,f,g,h,i,coucou); */
/*      return; */
/* } */

int fact(int n)
{
     if(n==0)
	  return(1);
     else
	  return (n*fact(n-1));
}

int main (int argc, char **argv)
{
     int x;
     printf("%d\n", (x=fact(10), ++x, x));
     printf("x=%d\n", x);
     return(0);
}
