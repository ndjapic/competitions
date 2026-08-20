program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a);

	if 400 mod a > 0 then
		writeln(-1)
	else
		writeln(400 div a);
end.
