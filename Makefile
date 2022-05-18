CFLAGS=-std=c11 -g -fno-common

RISCV_GCC=riscv64-unknown-linux-gnu-gcc

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

# Stage 1

chibicc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): chibicc.h

test/%.exe: chibicc test/%.c
	./chibicc -Iinclude -Itest -o test/$*.o test/$*.c
	$(RISCV_GCC) -static -o $@ test/$*.o -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; spike --isa rv64gc $$(which pk) ./$$i || exit 1; echo; done
	test/driver.sh

# Stage 2

stage2/chibicc: $(OBJS:%=stage2/%)
	$(RISCV_GCC) -static $(CFLAGS) -o $@ $^ $(LDFLAGS) -lm

stage2/%.o: chibicc %.c
	mkdir -p stage2/test
	./chibicc -o $(@D)/$*.o $*.c

clean:
	rm -rf chibicc tmp* $(TESTS) test/*.s test/*.exe stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean