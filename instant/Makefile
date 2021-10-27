all: LLVM

LLVM:
	stack build
	cp "`stack path --local-install-root`/bin/instant-exe" insc_llvm

clean:
	rm insc_llvm
