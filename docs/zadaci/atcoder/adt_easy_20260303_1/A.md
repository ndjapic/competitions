# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	ab, ac, bc: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ab, ac, ac, bc, bc);

	if ab = '<' then begin
		if ac = '>' then
			writeln('A')
		else if bc = '<' then
			writeln('B')
		else
			writeln('C');
	end else begin
		if bc = '>' then
			writeln('B')
		else if ac = '<' then
			writeln('A')
		else
			writeln('C');
	end;
end.

```
