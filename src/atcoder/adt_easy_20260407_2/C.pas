program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 1000;
var
	n, m, i, j, b, ans: int32;
	c: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do read(c[j]);
	readln;

	ans := 0;
	for i := 1 to n do begin
		readln(j, b);
		b := min(b, c[j]);
		inc(ans, b);
		dec(c[j], b);
	end;

	writeln(ans);
end.
