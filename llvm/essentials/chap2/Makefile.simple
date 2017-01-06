LLVM_DIR= $(HOME)/soft/llvm-3.8.1.src/bin/
LLVM_BIN=$(LLVM_DIR)/bin
LLVM_INCLUDE=$(LLVM_DIR)/include
LLVM_MODULES=core
LLVM_CXXFLAGS=$(shell $(LLVM_BIN)/llvm-config --cxxflags)
LLVM_LDFLAGS=$(shell $(LLVM_BIN)/llvm-config --ldflags)
LLVM_LIBS=$(shell $(LLVM_BIN)/llvm-config --libs --system-libs $(LLVM_MODULES))

CXX=clang++
LD=$(CXX)

all: run

run: simple.x
	./simple.x

simple.o: simple.cpp
	$(CXX) $< -c -o $@  $(LLVM_CXXFLAGS)

simple.x: simple.o
	$(LD) $^  -o $@ $(LLVM_LDFLAGS) $(LLVM_LIBS)
 
clean:
	/bin/rm -f simple.o simple.x
