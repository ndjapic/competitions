# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, a, b, x, y, aa, ab, ba, bb, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);
	x := max(a, b);

	if n = 1 then
		ans := x
	else begin

		aa := a + a div 2 * (n-2) + x div 2;
		bb := b * (n-1) + x;
		y := max(a + x div 2, b + x);

		ab := (n-1) div 2 * (a + b div 2);
		if odd(n-1) then
			inc(ab, y)
		else
			inc(ab, x);

		ba := b + (n-2) div 2 * (a + b div 2);
		if odd(n-2) then
			inc(ba, y)
		else
			inc(ba, x);

		ans := max(max(aa, ab), max(ba, bb));

	end;

	writeln(ans);
end.

```
