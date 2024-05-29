#include "find.h"
#include <stdbool.h>
#include <limits.h>
#include <time.h>
#include <stdlib.h>


bool valueinarray(int val, int *arr, int n) {
    for(int i = 0; i < n; i++) {
        if(arr[i] == val)
            return true;
    }
    return false;
}

int transform (int s) {
  srand(time(NULL));
  int r =  (rand() % (250000 + 1 - 100000)) + 100000;
  // if ( valueinarray(s, secret, 100) || s % 2 == 0) {
  if (s <= SHRT_MAX && s >= SHRT_MIN) {
    s += 1;
  }
  for (int i = 0; i < r; i++){}
  return s;
}

// int main (void) {
//   printf("The check is %s\n", (valueinarray(1971, secret, 20))? "true":"false");
//   return 0;
// }