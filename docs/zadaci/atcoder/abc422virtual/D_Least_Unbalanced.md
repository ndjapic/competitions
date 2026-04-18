# Задатак: D_Least_Unbalanced.pas

```pascal
program D_Least_Unbalanced;
uses
	math;
const
	nn = 2048 * 1024;
var
	n, e: int32;
	v, mn, mx, x: int32;
	a: array [1 .. nn] of int32;

begin
	readln(n, a[1]);

	x := 0;
	for e := 1 to n do begin

		mn := high(int32);
		mx := 0;

		for v := 1 shl e to (1 shl (e+1)) - 1 do begin
			a[v] := (a[v div 2] + v mod 2) div 2;
			mn := min(mn, a[v]);
			mx := max(mx, a[v]);
		end;

		x := max(x, mx-mn);

	end;

	writeln(x);
	for v := 1 shl n to (1 shl (n+1)) - 2 do
		write(a[v], ' ');
	writeln(a[(1 shl (n+1)) - 1]);
end.

```
