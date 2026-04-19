# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n: int32;
	d: int8;
	c: array [0 .. 9] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for d := 0 to 9 do c[d] := 0;

	while n > 0 do begin
		inc(c[n mod 10]);
		n := n div 10;
	end;

	if (c[1] = 1) and (c[2] = 2) and (c[3] = 3) then
		writeln('Yes')
	else
		writeln('No');
end.

```
