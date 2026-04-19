# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	m, d: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(m, d);

	if (m = 1) and (d = 7) then
		writeln('Yes')
	else if (m = 3) and (d = 3) then
		writeln('Yes')
	else if (m = 5) and (d = 5) then
		writeln('Yes')
	else if (m = 7) and (d = 7) then
		writeln('Yes')
	else if (m = 9) and (d = 9) then
		writeln('Yes')
	else
		writeln('No');
end.

```
