# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	notc, tci, p, q, n, m, x, y: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function isqrt(a: int32): int32;
var
	x, y: int32;
begin
	x := a;
	y := 1;
	while x > y do begin
		x := (x + y) div 2;
		y := a div x;
	end;
	isqrt := x;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin
		readln(p, q);

		x := 2*p + 4*q + 1;
		y := isqrt(x);
		if not odd(y) then dec(y);

		while x mod y > 0 do dec(y, 2);

		n := y div 2;
		m := x div y div 2;

		if (n > 0) and (p >= m-n) then
			writeln(n, ' ', m)
		else
			writeln(-1);
	end;
end.

```
