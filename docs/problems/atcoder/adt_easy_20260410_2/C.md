# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, m: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function confusing(m: int32): boolean;
var
	h, a, b, c, d: int8;
begin
	h := m div 60;
	m := m mod 60;

	a := h div 10;
	b := h mod 10;
	c := m div 10;
	d := m mod 10;

	h := 10 * a + c;
	m := 10 * b + d;

	confusing := (h < 24) and (m < 60);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, m);
	inc(m, 60 * h);

	while not confusing(m) do m := (m + 1) mod (24 * 60);

	writeln(m div 60, ' ', m mod 60);
end.

```
