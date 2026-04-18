# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, r, i, t: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, r);

	ans := int64(m) * r;
	for i := 1 to n do begin
		read(t);
		inc(ans, t);
	end;
	readln;

	writeln(ans);
end.

```
