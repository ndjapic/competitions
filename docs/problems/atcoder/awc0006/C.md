# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, d, t, i: int32;
	s: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, d);
	s := 0;

	for i := 1 to n do begin
		read(t);
		if t > m then
			inc(s, (t-m+d-1) div d);
	end;
	readln;

	writeln(s);
end.

```
