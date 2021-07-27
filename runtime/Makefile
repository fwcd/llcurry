MODULES = runtime prelude_external
LL = $(MODULES:%=$(CURDIR)/%.ll)

.PHONY: all
all: $(LL)

%.ll: %.c
	clang -S -emit-llvm -Wall $<
