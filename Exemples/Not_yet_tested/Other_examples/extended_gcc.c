#ifdef MCC
#else
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif

int* program_name;

int extended_gcd(int a,int b){

     int y,lasty,quotient,temp1,temp2;

     y = 1;
     lasty = 0;

     while(b != 0){

	  quotient = a / b;
	  temp1 = a%b;
	  a = b;
	  b = temp1;

	  temp2 = y;
	  y = lasty - (quotient*y);
	  lasty = temp2;

     } //End of while

     return(lasty);
} // End of function

int print_help() {
     fprintf(stderr, "%s%s%s\n", "Usage: ",program_name," phi e");
     exit(1);
     return(0);
}

int check_argument(int checker) {
     /* printf("checker = %d", checker); */
     if (checker != 0)
     {
	  fprintf(stderr, "%s\n", "ERROR bad arguments! Check your arguments again!");
	  print_help();
     }
     return(0);
}


int main(int argc, int **argv) {
     //Local variables
     int phi,e,g;
     int* endpt;
     phi = 0;
     e = 0;
     program_name = malloc(8*35);
     endpt = malloc(8);
     //Main function
     strcpy(program_name, argv[0]);
     if (argc > 3 || argc < 3)
     {
	  print_help();
     }

     //Convert the values from argv[1] and argv[2]
     phi = strtoull(argv[1],endpt, 10);
     //Check argument if endpt points to '\0' (Only numbers)
     /* check_argument(endpt[0]); */
     printf("phi = %d", phi);
     //Same as above
     e = strtoull(argv[2], endpt, 10);
     /* check_argument(endpt[0]); */
     //Function call
     g = extended_gcd(phi,e);
     if(g < 0) { //Check if number if negative. If so then make it positive.
	  int i;
	  i = 1;
	  while(i > 0 && g > 0)
	  {
	       g = g + (i*phi);
	       i++;
	  }
     }
     /* printf("%s%llu\n","Invers: ", g); */
     printf("%s%d\n","Invers: ", g);
     return 0;
}
