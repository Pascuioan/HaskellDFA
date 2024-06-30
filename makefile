output: out.hs
	ghc out.hs -o dfa.exe
	del *.o *.hi

generator: Generator.hs 
	ghc Generator.hs
	del *.o *.hi

clean:
	del *.o *.hi