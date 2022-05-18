CFLAGS=-std=c11 -g -fno-common

RISCV_GCC=riscv64-unknown-linux-gnu-gcc

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): chibicc.h

test/macro.exe: chibicc test/macro.c
	./chibicc -Iinclude -Itest -o test/macro.o test/macro.c
	$(RISCV_GCC) -static -o $@ test/macro.o -xc test/common

test/%.exe: chibicc test/%.c
	./chibicc -o test/$*.o test/$*.c
	$(RISCV_GCC) -static -o $@ test/$*.o -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; spike --isa rv64gc $$(which pk) ./$$i || exit 1; echo; done
	test/driver.sh

clean:
	rm -rf chibicc tmp* $(TESTS) test/*.s test/*.exe
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean