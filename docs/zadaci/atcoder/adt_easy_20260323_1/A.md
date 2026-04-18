# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	v, a, b, c: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(v, a, b, c);

	v := v mod (a+b+c);

	if v < a then
		writeln('F')
	else if v < a+b then
		writeln('M')
	else
		writeln('T')
end.

```
