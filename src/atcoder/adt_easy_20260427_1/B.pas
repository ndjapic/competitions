program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	w, b: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function f(x: int32): int32;
begin
	result := sqr(x) + 2 * x + 3;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(w, b);
	writeln(w * 1000 div b + 1);
end.
