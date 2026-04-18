# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, m, i, l, r: int32;
	a: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for l := 1 to n do a[l] := 0;

	for i := 1 to m do begin
		readln(l, r);
		inc(a[l-1]);
		dec(a[r]);
	end;

	for l := 0 to n-1 do begin
		write(a[l]);
		if l < n-1 then write(' ');
		inc(a[l+1], a[l]);
	end;
	writeln;
end.

```
