# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	sysutils;
var
	s: string;
	n: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := strtoint(copy(s, 4, 3));

	if (1 <= n) and (n <= 349) and (n <> 316) then
		writeln('Yes')
	else
		writeln('No');
end.

```
