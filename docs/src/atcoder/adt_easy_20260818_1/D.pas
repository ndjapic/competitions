program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, m, i, j, b: int8;
	c: array [1 .. NN] of int8;
	s: array [1 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 1 to m do begin
		c[j] := 0;
		s[j] := 0;
	end;

	for i := 1 to n do begin
		readln(j, b);
		inc(c[j]);
		inc(s[j], b);
	end;

	for j := 1 to m do
		writeln(s[j] / c[j] :0:5);
end.
