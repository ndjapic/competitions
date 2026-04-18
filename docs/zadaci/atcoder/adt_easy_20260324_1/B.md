# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, m, i: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	s := '3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679';
	readln(n);

	for i := 1 to n+2 do write(s[i]);
	writeln;
end.

```
