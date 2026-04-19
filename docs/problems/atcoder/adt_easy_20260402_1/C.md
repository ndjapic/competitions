# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function f(x: int32): int32;
var
	d: int8;
begin
	result := 0;
	while x > 0 do begin
		d := x mod 10;
		inc(result, sqr(d));
		x := x div 10;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to 100 do n := f(n);

	if n = 1 then
		writeln('Yes')
	else
		writeln('No');
end.

```
