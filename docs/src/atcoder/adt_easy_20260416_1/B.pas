program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	i := 1;
	while (i < n) and (s[i] <> s[i+1]) do inc(i);

	if i = n then
		writeln('Yes')
	else
		writeln('No');
end.
