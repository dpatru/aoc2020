
1: 01 01.input.txt
	time ./01 <  01.input.txt

test1: 01 01.test
	time ./01 < 01.test

01: 01.hs
	ghc 01.hs
