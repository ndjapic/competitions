# Problem: C.pas

```pascal
program _C;
uses
	math;
const
	nn = 200 * 1000;
var
	n, k, i: int32;
	ans: int64;
	a, b: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	readln(n, k);

	for i := 1 to n do read(a[i]);
	readln;

	b[1] := a[1];
	for i := 2 to n do
		b[i] := max(a[i], b[i-1] - k);

	for i := n-1 downto 1 do
		b[i] := max(b[i], b[i+1] - k);

	ans := 0;
	for i := 1 to n do inc(ans, b[i] - a[i]);

	writeln(ans);
end.

```
