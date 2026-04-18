# Задатак: E_Adjacent_XOR.pas

```pascal
program E_Adjacent_XOR;
{$MODE DELPHI}
uses
	Generics.Defaults, Generics.Collections, math;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i, l, r: int32;
	ans: boolean;
	a, b: array [1 .. nn] of int32;
	x: array [0 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;
		for i := 1 to n do read(b[i]); readln;

		x[0] := 0;
		for i := 1 to n do x[i] := x[i-1] xor a[i];

		r := 0;
		l := 1;
		ans := true;
		while (l <= n) and ans do begin
			r := max(l, r);
			if r = l then
				while (r <= n) and (x[r] xor x[l-1] <> b[l]) do inc(r);
			ans := (r <= n) and (x[r] xor x[l-1] = b[l]);
			inc(l);
		end;

		if ans then
			writeln('YES')
		else
			writeln('NO');

	end;
end.

```
