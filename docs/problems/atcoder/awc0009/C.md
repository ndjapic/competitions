# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, i, t, k, d, ans: int32;
	h: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, t, k);

	d := 1 shl 30;
	for i := 1 to n do begin
		read(h[i]);
		d := min(d, h[i]-1);
	end;
	readln;

	ans := 0;
	for i := 1 to n do
		if h[i]-d <= t+k then inc(ans);

	writeln(ans);
end.

```
