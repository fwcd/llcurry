SRC = $(CURDIR)/runtime.c
LL = $(CURDIR)/runtime.ll

.PHONY: all
all: $(LL)

$(LL): $(SRC)
	clang -S -emit-llvm $(SRC)
