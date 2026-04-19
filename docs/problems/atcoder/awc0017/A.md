# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, k, i, c, d: int32;
	s: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	s := 0;

	for i := 1 to n do begin
		readln(c, d);
		if c <= k then inc(s, d);
	end;

	writeln(s);
end.

```
