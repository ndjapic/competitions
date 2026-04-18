program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, m, i, j, q, x: int32;
	s: int64;
	p: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, s);

	for i := 1 to n do read(p[i]); readln;

	for j := 1 to m do begin
		readln(i, q);
		x := p[i] * q;
		inc(s, (x+1) div 2);
	end;

	writeln(s);
end.
