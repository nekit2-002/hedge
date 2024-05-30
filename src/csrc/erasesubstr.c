#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

char* erase_with_cspan(char* str, char* separators){
  int skip = strcspn(str, separators);
  int len = strlen(str);
  if (skip == len) skip = 0;
  int diff = len + 1 - skip;
  char* copy = (char*)malloc(len + 1);
  copy = strcpy(copy, str);
  copy += skip;
  str = realloc(str, diff);
  for (int i = 0; i < diff; i++){
    str[i] = copy[i];
  }

  return str;
}

char* erase_with_strpbrk(char* str, char* separators){
  char* res = strpbrk(str, separators);
  return res ? res : str;
}