# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b: int8;
	ch: char;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(a, b);

	ch := s[a];
	s[a] := s[b];
	s[b] := ch;

	writeln(s);
end.

```
