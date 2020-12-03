
1: 01 01.input.txt
	time ./01 <  01.input.txt

test1: 01 01.test
	time ./01 < 01.test

01: 01.hs
	ghc 01.hs

2: 02 02.input.txt
	time ./02 < 02.input.txt

02: 02.hs
	ghc 02.hs

