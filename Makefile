SRCS = $(wildcard src/*.hs)
CFILES = $(wildcard src/*.c)
TESTERS = $(wildcard test/*.hs)
LIB_DIR = ./src
TEST_DIR= ./test
TEST_EXE = ${TEST_DIR}/Spec
LIB_OBJS = $(wildcard ${LIB_DIR}/*.o)

library: ${SRCS}
	gcc -c ${CFILES}
	mv *.o ${LIB_DIR}

generate_tester: ./test/Spec.hs library
	stack ghc ./test/Spec.hs ${TESTERS} ${LIB_DIR}/palindrome.o ${SRCS}

clean:
	rm ./src/*.o
	rm ./src/*.hi
	rm ./test/*.o
	rm ./test/*.hi
	rm ./test/Spec
