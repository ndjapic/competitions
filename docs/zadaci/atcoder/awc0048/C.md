# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, k, l, r, d, ans: int32;
	h: array [1 .. nn] of int64;
	dif: array [0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for i := 1 to n do read(h[i]); readln;
	for i := 0 to n do dif[i] := 0;

	for k := 1 to m do begin
		readln(l, r, d);
		inc(dif[l-1], d);
		dec(dif[r], d);
	end;

	ans := 0;
	for i := 1 to n do begin
		dec(h[i], dif[i-1]);
		inc(dif[i], dif[i-1]);
		if h[i] > 0 then inc(ans);
	end;

	writeln(ans);
end.

```
