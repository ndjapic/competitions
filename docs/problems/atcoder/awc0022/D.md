# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000 + 1;
var
	n, k, i, ans: int32;
	a, b: array [0 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	a[0] := 0;
	for i := 1 to n do read(a[i]); readln;
	a[n+1] := 0;

	for i := 0 to n do b[i] := a[i] xor a[i+1];

	for i := 0 to n-k do b[i+k] := b[i] xor b[i+k];

	ans := 0;
	for i := 0 to n-k do inc(ans, b[i]);

	for i := n-k+1 to n do
		if b[i] > 0 then ans := -1;

	writeln(ans);
end.

```
