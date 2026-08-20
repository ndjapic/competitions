program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #math #function
var
	t: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function f(x: int32): int32;
begin
	result := x * (x+2) + 3;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(t);

	writeln( f(f(f(t) + t) + f(f(t))) );
end.
