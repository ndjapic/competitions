# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	cc = 100 * 100;
var
	notc, tci, n, i, j, a, mx: int32;
	c: array [1 .. cc] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for a := 1 to n*n do c[a] := 0;

		mx := 0;
		for i := 1 to n do begin
			for j := 1 to n do begin
				read(a);
				inc(c[a]);
				mx := max(mx, c[a]);
			end;
			readln;
		end;

		if mx <= n*n-n then
			writeln('YES')
		else
			writeln('NO');

	end;
end.

```
