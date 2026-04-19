# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function isqrt(a: int64): int64;
var
    x: int64;
begin
    x := min(a, high(int32));
    while x * x > a do
        x := (x + a div x) div 2;
    isqrt := x;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	ans := 0;
	while n > 1 do begin
		n := n div 2;
		inc(ans, (isqrt(n) + 1) div 2);
	end;

	writeln(ans);
end.

```
