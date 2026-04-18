# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, k, ans: int32;
	s: int64;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	s := 0;
	ans := 0;
	for i := 1 to k-1 do begin
		read(a[i]);
		inc(s, a[i]);
	end;

	for i := k to n do begin
		read(a[i]);
		inc(s, a[i]);
		if s <= 0 then inc(ans);
		dec(s, a[i-k+1]);
	end;
	readln;

	writeln(ans);
end.

```
