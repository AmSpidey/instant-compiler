all: LLVM

LLVM:
	stack build
	cp "`stack path --local-install-root`/bin/insc_llvm" insc_llvm
	cp "`stack path --local-install-root`/bin/insc_jvm" insc_jvm

clean:
	rm insc_llvm
	rm insc_jvm
