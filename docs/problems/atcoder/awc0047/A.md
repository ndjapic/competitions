# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, i, k, ans: int32;
	t: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	read(t[1]);
	ans := 0;

	for i := 2 to n do begin
		read(t[i]);
		if abs(t[i] - t[i-1]) >= k then inc(ans);
	end;
	readln;

	writeln(ans);
end.

```
