# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, x: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	for i := 1 to n-1 do
		if a[i] > a[i+1] then begin
			x := a[i+1];
			a[i+1] := a[i];
			a[i] := x;
		end;

	for i := 1 to n-2 do
		if a[i] > a[i+1] then begin
			x := a[i+1];
			a[i+1] := a[i];
			a[i] := x;
		end;

	writeln(a[n] + a[n-1]);
end.

```
