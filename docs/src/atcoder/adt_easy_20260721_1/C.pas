program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	XX = 100;
var
	n, i, x, s: int32;
	score: real;
	c: array [0 .. XX] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	s := 0;
	for x := 0 to XX do c[x] := 0;

	for i := 1 to 5*n do begin
		read(x);
		inc(s, x);
		inc(c[x]);
	end;
	readln;

	x := 0;
	for i := 1 to n do begin
		while c[x] = 0 do inc(x);
		dec(s, x);
		dec(c[x]);
	end;

	x := XX;
	for i := 1 to n do begin
		while c[x] = 0 do dec(x);
		dec(s, x);
		dec(c[x]);
	end;

	score := s / (3*n);
	writeln(score:0:5);
end.
