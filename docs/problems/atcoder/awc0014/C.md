# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	g, d, k, v: int32;
	m: int64;
	one: extended;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(g, m, d, k, v);
	one := 1.0;

	if int64(d) * k > g then
		ans := one * g * d <= one * (m-g) / v
	else
		ans := one * (1-d) * k + g <= one * (m-g) / v;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.

```
