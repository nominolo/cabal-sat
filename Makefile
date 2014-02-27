default: Sat

CXX = clang++
GHC ?= ghc
Q ?= @
MKTEMP ?= mktemp
PWD:=$(shell pwd)

#
# Clean everything, including minisat sources
#
.PHONY: clean
clean:
	find minisat -name '*.o' -delete
	find minisat -name '*.o[dr]' -delete
	find cbits -name '*.o' -delete
	find lib -name '*.hi' -delete
	find lib -name '*.o' -delete
	rm -f Sat

MINISAT_SRCS:=$(shell find minisat -name '*.cc')
MINISAT_HDRS:=$(shell find minisat -name '*.h')

foo:
	@echo "$(MINISAT_SRCS) $(MINISAT_HDRS)"

#
# Build the bits of Minisat that we need
#
minisat/core/Main.or minisat/core/Solver.or: $(MINISAT_SRCS) $(MINISAT_HDRS)
	make -C minisat/core Solver.or MROOT=$(PWD)/minisat
	make -C minisat/simp SimpSolver.or MROOT=$(PWD)/minisat

#
# Our C wrapper around Minisat
#
cbits/wrapper.o: cbits/wrapper.cc cbits/wrapper.h minisat/core/Solver.h minisat/simp/SimpSolver.h
	$(CXX) -c $< -Iminisat -o $@

#
# The solver binary
#
Sat: minisat/core/Solver.or minisat/core/Solver.or
Sat: lib/Sat.hs cbits/wrapper.o lib/Minisat.hs lib/PackageIndex.hs 
	$(GHC) --make $^ -Icbits -ilib -icabal-install -lstdc++ -o $@ -optP-include -optPautohack/cabal_macros.h

#
# Create a fresh checkout and build that.  Hopefully, that'll catch
# most "bad commit" style issues.
#
.PHONY: fresh-build
fresh-build:
	$(Q) ./utils/fresh-build.sh `pwd`
