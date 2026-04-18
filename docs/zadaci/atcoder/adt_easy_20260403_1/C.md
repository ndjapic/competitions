# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1000;
var
	n, i, j, k: int32;
	x, y: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function sqrd(i, j: int8): int32;
begin
	Result := sqr(x[i] - x[j]) + sqr(y[i] - y[j]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do readln(x[i], y[i]);

	for i := 1 to n do begin
		k := 1;
		for j := 1 to n do
			if sqrd(j, i) > sqrd(k, i) then k := j;
		writeln(k);
	end;
end.

```
