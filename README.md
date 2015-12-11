# Abstract Interpretation
Context-sensitive control flow analysis (k-CFA) for my fJS language with Abstract Interpretation approach.

## Demo
Program "twice.js":  
```
function main (arg) {
	var f = function (x) x;
	f(f)(function (y) y)
}
```

Control Flow Facts:
```
> cabal run
Preprocessing executable 'AbstractInterpretation' for
AbstractInterpretation-0.1.0.0...
Running AbstractInterpretation...
Program:
var f = function (x) {x @<1> } @<2> ;
f @<3> (f @<4> ) @<5> (function (y) {y @<6> } @<7> ) @<8>  @<9>

Cache:
1 :
 at [5] -> [Closure function (x) ... @2 bind []] at [8] -> [Closure function (y) ... @7 bind []]
2 :
 at [] -> [Closure function (x) ... @2 bind []]
3 :
 at [] -> [Closure function (x) ... @2 bind []]
4 :
 at [] -> [Closure function (x) ... @2 bind []]
5 :
 at [] -> [Closure function (x) ... @2 bind []]
7 :
 at [] -> [Closure function (y) ... @7 bind []]
8 :
 at [] -> [Closure function (y) ... @7 bind []]
9 :
 at [] -> [Closure function (y) ... @7 bind []]

Envir:
f at [] : [Closure function (x) ... @2 bind []]
```

Program "recursive.js":

```
function f (x) {
	if (x == 1) {
		1
	} else {
		x * f(x - 1)
	}
}

function main (arg) {
	f(10)
}
```

Control Flow Facts:
```
> cabal run
Preprocessing executable 'AbstractInterpretation' for
AbstractInterpretation-0.1.0.0...
Running AbstractInterpretation...
Program:
var f = function (x) {if (x @<1> ==1 @<2>  @<3> ) {1 @<4> } else {x @<5> *f @<6> (x @<7> -1 @<8>  @<9> ) @<10>  @<11> } @<12> } @<13> ;
f @<14> (10 @<15> ) @<16>  @<17>

Cache:
1 :
 at [10] -> [] at [16] -> []
5 :
 at [10] -> [] at [16] -> []
6 :
 at [10] -> [Closure function (x) ... @13 bind [("f",[])]] at [16] -> [Closure function (x) ... @13 bind [("f",[])]]
7 :
 at [10] -> [] at [16] -> []
10 :
 at [10] -> [] at [16] -> []
12 :
 at [10] -> [] at [16] -> []
13 :
 at [] -> [Closure function (x) ... @13 bind [("f",[])]]
14 :
 at [] -> [Closure function (x) ... @13 bind [("f",[])]]
16 :
 at [] -> []
17 :
 at [] -> []

Envir:
f at [] : [Closure function (x) ... @13 bind [("f",[])]]
x at [10] : []
x at [16] : []
```
