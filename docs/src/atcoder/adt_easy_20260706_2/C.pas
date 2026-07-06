program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	NN = 100;
var
	n, i: int8;
	a, b: array [1 .. NN] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]); readln;
	for i := 1 to n do read(b[i]); readln;

	i := 1;
	while (i <= n) and (b[a[i]] = i) do inc(i);

	if i > n then
		writeln('Yes')
	else
		writeln('No');
end.
