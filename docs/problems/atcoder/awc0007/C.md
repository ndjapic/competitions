# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 500 * 1000;
var
	n, k, i, ans: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	ans := 1;
	for i := 1 to n do begin
		read(a[i]);
		if a[ans] < a[i] then ans := i;
	end;
	readln;

	writeln(ans);
end.

```
