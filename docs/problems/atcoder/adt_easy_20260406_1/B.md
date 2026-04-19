# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	i, a, b: int8;
	s: int16;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	s := 1;
	for i := 1 to 9 do begin
		read(a);
		inc(s, a);
	end;
	readln;

	for i := 1 to 8 do begin
		read(b);
		dec(s, b);
	end;
	readln;

	writeln(max(s, 0));
end.

```
