int fact(int n, int a, int b, int c, int d, int e, int f, int g, int h)
{
     if(n==0)
     {
	  return(1);
     }
     else
     {
	  return(n*fact(n-1));
     }
}



int main (int argc, char **argv)
{
     int i;
     for(i=0;i<100;i++)
     {
	  printf("fact(%d)=%d\n", i, fact(i));
     }
     return(0);
}
