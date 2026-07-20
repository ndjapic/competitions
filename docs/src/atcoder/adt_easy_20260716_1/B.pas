program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, c, x: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c);

	x := b - b mod c;
	if x < a then x := -1;

	writeln(x);
end.
