# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	if s = 'Monday' then
		writeln(5)
	else if s = 'Tuesday' then
		writeln(4)
	else if s = 'Wednesday' then
		writeln(3)
	else if s = 'Thursday' then
		writeln(2)
	else if s = 'Friday' then
		writeln(1);
end.

```
