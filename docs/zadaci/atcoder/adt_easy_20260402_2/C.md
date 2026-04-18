# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, a, i: int32;
	t: array [0 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a);
	t[0] := -a;

	for i := 1 to n do begin
		read(t[i]);
		t[i] := max(t[i]+a, t[i-1]+a);
		writeln(t[i]);
	end;
	readln;
end.

```
