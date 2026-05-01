program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	if x < 40 then
		writeln(40 - x)
	else if x < 70 then
		writeln(70 - x)
	else if x < 90 then
		writeln(90 - x)
	else
		writeln('expert');
end.
