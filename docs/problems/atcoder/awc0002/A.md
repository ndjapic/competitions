# Problem: A.pas

```pascal
program _A;
{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, k, i: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(a[i]); readln;

	i := 1;
	while (i <= n) and (a[i] <> k) do inc(i);

	if i > n then i := -1;
	writeln(i);
end.

```
