# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w, i, j: int8;
	s: array [1 .. 2] of string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);
	setlength(s[1], w);
	setlength(s[2], w);

	for j := 1 to w do s[1][j] := '#';
	for j := 2 to w-1 do s[2][j] := '.';
	s[2][1] := '#';
	s[2][w] := '#';

	writeln(s[1]);
	for i := 2 to h-1 do writeln(s[2]);
	writeln(s[1]);
end.

```
