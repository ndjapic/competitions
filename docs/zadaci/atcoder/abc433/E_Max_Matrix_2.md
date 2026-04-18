# Задатак: E_Max_Matrix_2.pas

```pascal
program E_Max_Matrix_2;
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, m, i, j: int32;
	a: array [1 .. nn] of array of int32;
	x, y: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(n, m);
		for i := 1 to n do read(x[i]); readln;
		for j := 1 to m do read(y[j]); readln;

		for i := 1 to n do setlength(a[i], m+1);
	end;
end.

```
