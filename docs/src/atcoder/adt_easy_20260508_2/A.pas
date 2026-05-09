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

	i := 1;
	while (i <= 3) and (s[i] <> 'R') do inc(i);
	while (i <= 3) and (s[i] <> 'M') do inc(i);

	if i <= 3 then
		writeln('Yes')
	else
		writeln('No');
end.
