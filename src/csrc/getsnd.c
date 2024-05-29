#include <string.h>
#include <stdbool.h>
#include <limits.h>
#include "struct.h"
#include <time.h>
#include <stdlib.h>

int getsnd (foo* s){
  int fst = s ->fst;
  int snd = s ->snd;
  srand(time(NULL));
  int r =  (rand() % (250000 + 1 - 100000)) + 100000;
  for (int i = 0; i < r; i++){}
  if (fst % 89 == 0 && snd % 97 == 0 && fst != 0 && snd != 0){return fst;}
  return snd;
}