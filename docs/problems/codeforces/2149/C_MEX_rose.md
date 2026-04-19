# Problem: C_MEX_rose.pas

```pascal
program C_MEX_rose;
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, k, ai, ans: int32;
	c: array [0 .. nn] of int32;

begin
	randomize;
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, k);

		for ai := 0 to n do c[ai] := 0;

		for i := 1 to n do begin
			read(ai);
			inc(c[ai]);
		end;
		readln;

		ans := 0;
		for ai := 0 to k-1 do
			if c[ai] = 0 then inc(ans);

		ans := max(ans, c[k]);
		writeln(ans);

	end;
end.

```
