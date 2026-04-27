program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function f(x: int32): int32;
begin
	result := sqr(x) + 2 * x + 3;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);
	writeln(s[n]);
end.
