#include <stdio.h>
void C_Library_Function(float send[], int count, float recv[]) {
      int i; 
      for(i=0;i<count;i++) {
          recv[i] = send[i] * (i+1);
      }
}
