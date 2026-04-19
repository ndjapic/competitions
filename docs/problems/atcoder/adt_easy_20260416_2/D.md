# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, k, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function ds(x: int32): int8;
begin
	result := 0;
	while x > 0 do begin
		inc(result, x mod 10);
		x := x div 10;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	ans := 0;
	for i := 1 to n do
		if ds(i) = k then inc(ans);
	writeln(ans);
end.

```
