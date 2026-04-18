# Задатак: D_Yet_Another_Array_Problem.pas

```pascal
program D_Yet_Another_Array_Problem;
const
	nn = 100 * 1000;
var
	notc, tci, n, i: int32;
	x, g: int64;

function gcd(a, b: int64): int64;
begin
	if b = 0 then
		gcd := a
	else
		gcd := gcd(b, a mod b);
end;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		g := 0;

		for i := 1 to n do begin
			read(x);
			g := gcd(g, x);
		end;
		readln;

		x := 2;
		while gcd(x, g) > 1 do inc(x);
		writeln(x);

	end;
end.

```
