program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #grid #counterclockwise #rotation
uses
	math;
const
	NN = 3000;
var
	n, i, j, x0, y0, x1, y1: int32;
	e: int8;
	a: array [1 .. NN] of string;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(a[i]);

	setlength(s, n);
	for i := 1 to n do begin
		for j := 1 to n do begin
			x0 := i;
			y0 := j;
			for e := 1 to min(min(i, j), min(n+1-i, n+1-j)) mod 4 do begin
				x1 := n+1-y0;
				y1 := x0;
				x0 := x1;
				y0 := y1;
			end;
			s[j] := a[x0][y0];
		end;
		writeln(s);
	end;
end.
