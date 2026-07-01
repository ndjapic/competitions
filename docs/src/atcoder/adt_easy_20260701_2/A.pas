program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i: int8;
	s: array [1 .. NN] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(s[i]);

	i := 2;
	while (i < n) and ((s[i-1][2] <> 'w') or (s[i][2] <> 'w')) do inc(i);

	if i < n then
		writeln('No')
	else
		writeln('Yes');
end.
