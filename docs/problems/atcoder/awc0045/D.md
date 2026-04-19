# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
	xx = 1000 * 1000;
var
	n, i, x, y: int32;
	mn, mx: int64;
	lpd: array [1 .. xx] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for x := 1 to xx do lpd[x] := x;

	for y := 2 to 1000 do
		if lpd[y] = y then begin
			x := y;
			while x <= xx do begin
				if lpd[x] = x then lpd[x] := y;
				inc(x, y);
			end;
		end;

	readln(n);

	mn := 0;
	mx := 0;
	for i := 1 to n do begin
		read(x);
		y := x;

		while y > 1 do begin
			inc(mn, x div y);
			y := y div lpd[y];
			inc(mx, y);
		end;
	end;
	readln;

	writeln(mn, ' ', mx);
end.

```
