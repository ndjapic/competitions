program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	i := 2;
	while (i <= n-1) and (s[i] = '=') do inc(i);

	if (i > n-1) and (s[1] = '<') and (s[n] = '>') then
		writeln('Yes')
	else
		writeln('No');
end.
