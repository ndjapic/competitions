program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i: int8;
	d: int32;
	t: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);

	for i := 1 to n do read(t[i]);
	readln;

	i := 2;
	while (i <= n) and (t[i] - t[i-1] > d) do inc(i);

	if i > n then
		writeln(-1)
	else
		writeln(t[i]);
end.
