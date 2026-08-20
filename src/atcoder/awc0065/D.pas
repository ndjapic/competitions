program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #unfinished
const
	nn = 100 * 1000;
	xx = 1000;
var
	n, i, x, y, c: int32;
	s0, s1, s2, s3: array [0 .. 2*xx+1] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for x := 0 to 2*xx+1 do begin
		s0[x] := 0;
		s1[x] := 0;
		s2[x] := 0;
		s3[x] := 0;
	end;

	for i := 1 to n do begin
		readln(x, y, c);
		inc(s3[x+y+1], c);
		inc(s2[x+y+1], c);
	end;
	readln;
end.
