
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

9: 09 09.input.txt
	time ./09 < 09.input.txt

test9 :: 09 09.test
	./09 < 09.test

09: 09.hs
	ghc 09.hs

FORCE: ;

10: FORCE
	ghc 10.hs
	time ./10 < 10.input.txt

test10a :: 10.hs 10.test0
	ghc 10.hs
	./10 < 10.test0

test10 :: 10.hs 10.test
	ghc 10.hs
	./10 < 10.test

11: FORCE
	ghc 11.hs
	time ./11 < 11.input.txt

test11a :: 11.hs 11.test0
	ghc 11.hs
	./11 < 11.test0

test11 :: 11.hs 11.test
	ghc 11.hs
	./11 < 11.test

12: FORCE
	ghc 12.hs
	time ./12 < 12.input.txt

test12a :: 12.hs 12.test0
	ghc 12.hs
	./12 < 12.test0

test12 :: 12.hs 12.test
	ghc 12.hs
	./12 < 12.test

13: FORCE
	ghc 13.hs
	time ./13 < 13.input.txt

test13a :: 13.hs 13.test0
	ghc 13.hs
	./13 < 13.test0

test13 :: 13.hs 13.test
	ghc 13.hs
	./13 < 13.test
