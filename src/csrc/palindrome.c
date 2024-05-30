#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "palindrome.h"

bool is_palindrome(char* s)
{
    for (int i = 0, j = strlen(s) - 1; i < j; ++i, --j)
        if (s[i] != s[j]) return false;
    return true;
}

char* reverse (char* s){
    int len = strlen(s);
    for(int i = 0, j = len - 1; i <= j; i++,j--){
        char c = s[i];
        s[i] = s[j];
        s[j] = c;
    }
    return s;
}

char* copy_palindrome (char* s) {
    char* copy = (char*)malloc((strlen(s) + 1)*sizeof(char));
    copy = strcpy(copy, s);
    return copy;
}