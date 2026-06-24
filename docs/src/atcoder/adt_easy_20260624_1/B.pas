program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	y: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(y);

	if y mod 4 > 0 then
		writeln('365')
	else if y mod 100 > 0 then
		writeln('366')
	else if y mod 400 > 0 then
		writeln('365')
	else
		writeln('366');
end.
