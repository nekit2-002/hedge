SRCS = $(wildcard ../src/*.hs)
CFILES = $(wildcard ../src/csrc/*.c)
TESTERS = $(filter-out Spec.hs, $(wildcard *.hs))
LIB_DIR = ./src
TEST_DIR= ./test
TEST_EXE = ${TEST_DIR}/Spec
OBJS = *.o

library: ${SRCS}
	gcc -c ${CFILES}

generate_tester: Spec.hs library
	stack ghc Spec.hs ${TESTERS} ${OBJS} ${SRCS} 

clean:
	rm ../src/*.o
	rm ../src/*.hi
	rm *.o
	rm *.hi

