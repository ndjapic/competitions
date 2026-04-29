program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s1, s2: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s1);
	readln(s2);

	if (s1[1] = '.') and (s2[2] = '.') then
		writeln('No')
	else if (s1[2] = '.') and (s2[1] = '.') then
		writeln('No')
	else
		writeln('Yes');
end.
