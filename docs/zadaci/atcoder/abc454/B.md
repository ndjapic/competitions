# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	n, m, i, f, c1: int8;
	c: array [1 .. nn] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for f := 1 to m do c[f] := 0;

	c1 := 0;
	for i := 1 to n do begin
		read(f);
		inc(c[f]);
		if c[f] = 1 then inc(c1);
	end;
	readln;

	if c1 = n then
		writeln('Yes')
	else
		writeln('No');

	if c1 >= m then
		writeln('Yes')
	else
		writeln('No');
end.

```
