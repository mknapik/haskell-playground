build:
	@find app/ src/ | entr stack build

exec:
	@find .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/playground-exe/playground-exe | entr stack exec playground-exe