
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

3: 03 03.input.txt
	time ./03 < 03.input.txt

03: 03.hs
	ghc 03.hs

4: 04 04.input.txt
	time ./04 < 04.input.txt

04: 04.hs
	ghc 04.hs

5: 05 05.input.txt
	time ./05 < 05.input.txt

05: 05.hs
	ghc 05.hs

6: 06 06.input.txt
	time ./06 < 06.input.txt

06: 06.hs
	ghc 06.hs

7: 07 07.input.txt
	time ./07 < 07.input.txt

07: 07.hs
	ghc 07.hs

8: 08 08.input.txt
	time ./08 < 08.input.txt

test8 :: 08 08.test
	./08 < 08.test

08: 08.hs
	ghc 08.hs

