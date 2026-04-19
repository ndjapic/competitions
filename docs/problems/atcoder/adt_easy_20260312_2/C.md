# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	mm = 100;
var
	n, m, i, j, x, y, mx: int8;
	s: array [1 .. mm] of string;
	score: array [1 .. mm] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do begin
		readln(s[i]);
		score[i] := 0;
	end;

	for j := 1 to m do begin

		x := 0;
		y := 0;
		for i := 1 to n do
			if s[i][j] = '0' then
				inc(x)
			else
				inc(y);

		if (x = 0) and (y = 0) then
			for i := 1 to n do inc(score[i])
		else if x < y then begin
			for i := 1 to n do
				if s[i][j] = '0' then inc(score[i]);
		end else begin
			for i := 1 to n do
				if s[i][j] = '1' then inc(score[i]);
		end;

	end;

	mx := 0;
	for i := 1 to n do mx := max(mx, score[i]);

	for i := 1 to n do
		if score[i] = mx then write(i, ' ');
	writeln;
end.

```
