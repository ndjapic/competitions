program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 16;
var
	i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	i := 2;
	while (i <= n) and (s[i] = '0') do inc(i, 2);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.
