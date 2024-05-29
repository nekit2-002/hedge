#include "substring.h"
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>


long* partialSums(int n, int arr[n]){
  long* res= (long*)malloc(n * sizeof(long));
  long psum = 0;
  for (int i = 0; i < n; i++){
    res[i] = arr[i] + psum;
    psum += arr[i];
  }

  return res;
}

long getSum (int n, int arr[n]){
  long sum = 0;
  for (int i = 0; i < n; i++){
    sum += arr[i];
  }
  return sum;
}

long sumFromChild(int n, int arr[n]){
  int fd[2];
  pid_t pid;
  long* res= (long*)malloc(n * sizeof(long));
  long s = 0;

  pipe(fd);

  pid = fork();
  if (pid == -1) {
    perror("fork");
    exit(EXIT_FAILURE);
  } else if (pid == 0) {
    close(fd[0]);
    res = partialSums(n, arr);
    s = res[n - 1];
    if (write(fd[1], &s, sizeof(s)) == -1){
       perror("write");
       exit(EXIT_FAILURE);
    };
    close(fd[1]);
    exit(EXIT_SUCCESS);
  } else {
    close(fd[1]);
    if (read(fd[0], &s, sizeof(s)) == -1) {
      perror("read");
      exit(EXIT_FAILURE);
    }

    close(fd[0]);
  }


  return s;
}