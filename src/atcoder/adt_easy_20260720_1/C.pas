program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);

	if pos(t, s) > 0 then
		writeln('Yes')
	else
		writeln('No');
end.
