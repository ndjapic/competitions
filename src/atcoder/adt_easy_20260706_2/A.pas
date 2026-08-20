program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	t, a: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(t);
	readln(a);

	i := 1;
	while (i <= n) and ((t[i] = 'x') or (a[i] = 'x')) do inc(i);

	if i <= n then
		writeln('Yes')
	else
		writeln('No');
end.
