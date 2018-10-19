#include <stdio.h>
#include <malloc.h>

typedef struct {
  int lenc, lenf;
  float *c, *f;
} pass;

void simulation(pass *arrays);

int main () {

   float a[2];
   int i;
   pass *arrays=(pass *)malloc(sizeof(pass));

   (*arrays).lenc = 2;
   arrays->c = malloc((*arrays).lenc*sizeof(float));
   
   a[0] = 10.0;
   a[1] = 20.0;

/* Initializing C array to be passed to Fortran routine "simulation" */
   for(i=0;i<(*arrays).lenc;i++) *(arrays->c+i)=a[i];
       
   printf("Your output is \n");
/* Calling Fortran routine "simulation" */
   simulation(arrays);

/* Printing Fortran array passed back from Fortran routine "simulation" */
   for(i=0;i<(*arrays).lenf;i++) printf("%f\n",*(arrays->f+i));

   printf("Expected output is \n");
   printf("   10.00000       20.00000\n");
   printf("10.00000\n");
   printf("20.00000\n");
   printf("30.00000\n");

   return 0;
}
