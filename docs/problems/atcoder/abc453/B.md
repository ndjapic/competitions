# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	t, x, i, i0: int32;
	a: array [0 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(t, x);
	read(a[0]);
	i0 := 0;
	writeln(i0, ' ', a[i0]);

	for i := 1 to t do begin
		read(a[i]);
		if abs(a[i] - a[i0]) >= x then begin
			i0 := i;
			writeln(i0, ' ', a[i0]);
		end;
	end;
	readln;
end.

```
