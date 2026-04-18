# Задатак: A_Alice_and_Bob.pas

```pascal
program A_Alice_and_Bob;
uses
	math;
const
	nn = 300 * 1000;
var
	notc, tci, n, a, b, i, l, r: int32;
	v: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(n, a);

		for i := 1 to n do read(v[i]); readln;

		l := 1;
		r := n;
		while (l <= n) and (v[l] < a) do inc(l);
		while (r > 0) and (v[r] > a) do dec(r);

		if l > n then
			b := a-1
		else if l-1 > n-r then
			b := a-1
		else
			b := a+1;

		writeln(b);
	end;
end.

```
