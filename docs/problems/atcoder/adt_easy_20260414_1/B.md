# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	r, x: int32;
	le: array [1 .. 2] of int32 = (1600, 1200);
	ri: array [1 .. 2] of int32 = (3000, 2400);
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, x);

	if (le[x] <= r) and (r < ri[x]) then
		writeln('Yes')
	else
		writeln('No');
end.

```
