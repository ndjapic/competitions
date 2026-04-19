# Problem: A.pas

```pascal
program A;
uses
	math;
const
	nn = 256 * 1024;
var
	n, i: int32;
	a: array [1 .. nn] of int32;

begin
	readln(n);
	for i := 1 to n do read(a[i]); readln;
end.

```
