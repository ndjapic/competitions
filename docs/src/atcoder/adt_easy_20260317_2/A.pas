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

	i := 3;
	while (i <= n) and not ((s[i-2] = 'A') and (s[i-1] = 'B') and (s[i] = 'C')) do
		inc(i);

	if i > n then
		writeln(-1)
	else
		writeln(i-2);
end.
