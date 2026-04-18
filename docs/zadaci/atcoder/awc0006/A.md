# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, l, w, d, i, c: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, l, w);
	c := 0;

	for i := 1 to n do begin
		read(d);
		if (l-w <= d) and (d <= l+w) then inc(c);
	end;
	readln;

	writeln(c);
end.

```
