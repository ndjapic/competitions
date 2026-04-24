program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	t, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function f(x: int32): int32;
begin
	result := sqr(x) + 2 * x + 3;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(t);

	ans := f(f(f(t) + t) + f(f(t)));

	writeln(ans);
end.
