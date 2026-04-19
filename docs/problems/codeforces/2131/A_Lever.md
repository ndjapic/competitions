# Problem: A_Lever.pas

```pascal
program A_Lever;
uses
	math;
var
	ntc, tci: int16;
	n, i, ans: int8;
	a, b: array [1 .. 10] of int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;
		for i := 1 to n do read(b[i]); readln;

		ans := 1;
		for i := 1 to n do inc(ans, max(a[i] - b[i], 0));

		writeln(ans);

	end;
end.

```
