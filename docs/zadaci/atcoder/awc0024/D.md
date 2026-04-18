# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, w, k, i, l: int32;
	c: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, w, k);

	for l := 0 to n do c[l] := 0;

	for i := 1 to k do begin
		readln(l);
		inc(c[l-1]);
		dec(c[l+w-1]);
	end;

	for l := 0 to n-1 do begin
		write(c[l], ' ');
		inc(c[l+1], c[l]);
	end;
	writeln;
end.

```
