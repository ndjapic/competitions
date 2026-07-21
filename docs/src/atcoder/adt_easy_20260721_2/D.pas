program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i: int8;
	a: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	i := 2;
	while (i < n) and (a[i-1] * a[i+1] = sqr(a[i])) do inc(i);

	if i >= n then
		writeln('Yes')
	else
		writeln('No');
end.
