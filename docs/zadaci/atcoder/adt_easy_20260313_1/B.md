# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, p1, p, ans: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	read(p1);

	ans := 0;
	for i := 2 to n do begin
		read(p);
		ans := max(ans, p+1-p1);
	end;
	readln;
	writeln(ans);
end.

```
