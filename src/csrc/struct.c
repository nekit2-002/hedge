#include <string.h>
#include <stdbool.h>
#include <limits.h>
#include "struct.h"
#include <time.h>
#include <stdlib.h>

int getfield(foo* s){
  srand(time(NULL));
  // int r =  (rand() % (250000 + 1 - 100000)) + 100000;
  int fst = s -> fst;
  int snd = s -> snd;
  for (int i = 0; i < 100; i++){}
  if (fst % 2 == 0 && snd % 2 == 0 && fst != 0 && snd != 0) return snd;
  return fst;
}