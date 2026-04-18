# Задатак: B_Yet_Another_MEX_Problem.pas

```pascal
program B_Yet_Another_MEX_Problem;
const
	nn = 200 * 1000;
var
	notc, tci, n, k, i, x: int32;
	c: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		for x := 0 to n do c[x] := 0;
		for i := 1 to n do begin
			read(x);
			inc(c[x]);
		end;
		readln;

		x := 0;
		for i := 1 to k-1 do
			if c[x] > 0 then inc(x);

		writeln(x);

	end;
end.

```
