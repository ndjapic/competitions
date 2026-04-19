# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 500 * 1000;
var
	n, i: int32;
	a: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for i := 1 to n do read(a[i]); readln;

	for i := n downto 1 do a[i] := a[a[i]];

	for i := 1 to n-1 do write(a[i], ' ');
	writeln(a[n]);
end.

```
