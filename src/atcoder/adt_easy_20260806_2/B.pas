program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a);

	if 400 mod a > 0 then
		b := -1
	else
		b := 400 div a;

	writeln(b);
end.
