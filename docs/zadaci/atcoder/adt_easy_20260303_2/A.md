# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	i, x, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	ans := 1;

	for i := 1 to 9 do begin
		read(x);
		inc(ans, x);
	end;
	readln;

	for i := 1 to 8 do begin
		read(x);
		dec(ans, x);
	end;
	readln;

	writeln(ans);
end.

```
