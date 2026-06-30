program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	b, g: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(b, g);

	if b > g then
		writeln('Bat')
	else
		writeln('Glove');
end.
