#include "substring.h"
#include <string.h>
#include <stdlib.h>

int substrpos (char* str, char* substr) {
  char* found = strstr(str, substr);
  return (found) ? (found - str): -1;
}

int copysubstrpos (char* str, char* substr){
  size_t len = strlen(str) + 1;
  char* copy = (char*)calloc(len, sizeof(char));
  copy = strcpy(copy, str);
  char* found = strstr(copy, substr);
  return (found) ? (found - copy): -1;
}