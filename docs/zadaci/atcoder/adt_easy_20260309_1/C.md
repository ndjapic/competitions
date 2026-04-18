# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	hh = 100;
var
	h, w, i, j, d: int8;
	n, k: int16;
	s: string;
	grid: array [0 .. hh, 0 .. hh] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w, n);
	setlength(s, w);

	for i := 0 to h-1 do
		for j := 0 to w-1 do grid[i, j] := 1;

	i := 0;
	j := 0;
	d := 0;

	for k := 1 to n do begin
		d := (d+4 + grid[i, j]) mod 4;
		grid[i, j] := - grid[i, j];
		case d of
			0: i := (i-1+h) mod h;
			1: j := (j+1+w) mod w;
			2: i := (i+1+h) mod h;
			3: j := (j-1+w) mod w;
		end;
	end;

	for i := 0 to h-1 do begin
		for j := 0 to w-1 do
			case grid[i, j] of
				+1: s[j+1] := '.';
				-1: s[j+1] := '#';
			end;
		writeln(s);
	end;
end.

```
