# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, i, x: int32;
	s, mn: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	mn := high(int32);
	s := 0;
	for i := 1 to n do begin
		read(x);
		inc(s, x);
		mn := min(mn, s);
	end;
	readln;

	writeln(max(-mn, 0) + s);
end.

```
