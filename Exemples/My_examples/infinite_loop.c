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
     i=0;
     while(fact(10,1,2,3,4,5,6,7,8),++i)
     {
	  /* printf("%d\n", i); */
	  fact(10,1,2,3,4,5,6,7,8);
     }
     return(0);
}
