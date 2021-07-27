SRC = $(CURDIR)/runtime.c $(CURDIR)/prelude.c
LL = $(CURDIR)/runtime.ll $(CURDIR)/prelude.ll

.PHONY: all
all: $(LL)

%.ll: %.c
	clang -S -emit-llvm -Wall $<
