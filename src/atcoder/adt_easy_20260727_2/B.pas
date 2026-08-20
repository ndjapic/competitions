program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i: int8;
	a: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	i := 3;
	while (i <= n) and not ((a[i-2] = a[i]) and (a[i-1] = a[i])) do inc(i);

	if i <= n then
		writeln('Yes')
	else
		writeln('No');
end.
