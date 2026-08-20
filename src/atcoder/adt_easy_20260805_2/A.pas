program _A;
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

	i := 2;
	while (i <= n) and not (
		(
			(s[i-1] = 'a') and (s[i] = 'b')
		) or (
			(s[i-1] = 'b') and (s[i] = 'a')
		)
	) do inc(i);

	if i <= n then
		writeln('Yes')
	else
		writeln('No');
end.
