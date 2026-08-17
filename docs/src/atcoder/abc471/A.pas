program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	if (a+b = 9) or (a-b = 9) or (a*b = 9) or (a = 9*b) then
		writeln('Nine')
	else
		writeln('Nein');
end.
