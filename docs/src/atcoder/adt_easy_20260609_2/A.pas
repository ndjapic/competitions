program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	i := 2;
	while (i <= 16) and (s[i] = '0') do inc(i, 2);

	if i <= 16 then
		writeln('No')
	else
		writeln('Yes');
end.
