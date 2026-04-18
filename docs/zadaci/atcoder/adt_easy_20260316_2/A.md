# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, i, x: int8;
	a: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do read(a[i]); readln;
	readln(x);

	i := 1;
	while (i <= n) and (a[i] <> x) do inc(i);

	if i <= n then
		writeln('Yes')
	else
		writeln('No');
end.

```
