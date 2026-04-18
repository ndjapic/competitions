# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, k, p, claps: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	claps := 0;
	for i := 1 to n do begin
		read(p);
		if p >= k then inc(claps, p);
	end;
	readln;

	writeln(claps);
end.

```
