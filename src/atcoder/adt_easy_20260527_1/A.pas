program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	if (200 <= s) and (s <= 299) then
		writeln('Success')
	else
		writeln('Failure');
end.
