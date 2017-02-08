# Build LLVM line
# cmake ../ -DCMAKE_INSTALL_PREFIX=$PWD/../bin/ -DCMAKE_CXX_FLAGS="-fpermissive" -DLLVM_TARGETS_TO_BUILD="X86" -DLLVM_ENABLE_THREADS=OFF -DCMAKE_BUILD_TYPE=Release
export PATH=$PWD/llvm-2.6/bin/bin:$PATH
. ~/.ghc7
ghc  -no-user-package-conf -package-conf .cabal-sandbox/x86_64-linux-ghc-7.4.2-packages.conf.d/ edsl1.hs && ./edsl1
