program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 4;
var
	i, a, ans: int8;
	c: array [1 .. n] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	for a := 1 to n do c[a] := 0;

	for i := 1 to n do begin
		read(a);
		inc(c[a]);
	end;
	readln;

	ans := 0;
	for a := 1 to n do
		inc(ans, c[a] div 2);

	writeln(ans);
end.
