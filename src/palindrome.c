#include <string.h>
#include <stdbool.h>
#include "palindrome.h"

bool is_palindrome(char* s)
{
    for (int i = 0, j = strlen(s) - 1; i < j; ++i, --j)
        if (s[i] != s[j]) return false;
    return true;
}