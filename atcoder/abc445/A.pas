program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	if s[1] = s[n] then
		writeln('Yes')
	else
		writeln('No');
end.
