# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	StrUtils;
var
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(s);
	readln(t);

	if ContainsText(s, t) then
		writeln('Yes')
	else
		writeln('No');
end.

```
