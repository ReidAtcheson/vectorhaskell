

opt : linearspace
	time ./linearspace 5 1000 10000 opt

unopt : linearspace
	time ./linearspace 5 1000 10000 unopt

linearspace : linearspace.hs
	ghc --make -Odph linearspace.hs



.PHONY : clean


clean :
	rm -rf ./linearspace.hi
	rm -rf ./linearspace.o
	rm -rf ./linearspace
