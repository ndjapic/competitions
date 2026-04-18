# Задатак: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100 * 1000;
var
	n, q, i, a, b: int32;
	s: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(s[i]);
	readln;

	for i := 1 to q do begin
		readln(a, b);
		if s[a] > s[b] then
			writeln('Yes')
		else
			writeln('No');
	end;
end.

```
