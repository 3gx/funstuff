LLVM_BIN=/usr/bin
LLVM_MODULES=core
LLVM_CXXFLAGS=$(shell $(LLVM_BIN)/llvm-config --cxxflags)
LLVM_LDFLAGS=$(shell $(LLVM_BIN)/llvm-config --ldflags)
LLVM_LIBS=$(shell $(LLVM_BIN)/llvm-config --libs --system-libs $(LLVM_MODULES))

CXX=clang++
LD=$(CXX)

PROG=prog

all: run

run: $(PROG).x
	./$(PROG).x

$(PROG).o: $(PROG).cpp
	$(CXX) $< -c -o $@  $(LLVM_CXXFLAGS)

$(PROG).x: $(PROG).o
	$(LD) $^  -o $@ $(LLVM_LDFLAGS) $(LLVM_LIBS)
 
clean:
	/bin/rm -f $(PROG).o $(PROG).x
