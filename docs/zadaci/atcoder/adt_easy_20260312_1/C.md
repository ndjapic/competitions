# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, d, i, r: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);
	readln(s);

	r := n;
	for i := 1 to d do begin
		while s[r] = '.' do dec(r);
		s[r] := '.';
	end;

	writeln(s);
end.

```
