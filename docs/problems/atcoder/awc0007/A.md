# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, m, i, j, ei, cj, e, c: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	e := high(int32);
	for i := 1 to n do begin
		read(ei);
		e := min(e, ei);
	end;
	readln;

	c := 0;
	for j := 1 to m do begin
		read(cj);
		inc(c, cj);
	end;
	readln;

	writeln(int64(e) * c);
end.

```
