# Задатак: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s, t, x: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s, t, x);

	if s > t then begin
		inc(t, 24);
		if x < s then inc(x, 24);
	end;

	if (s <= x) and (x < t) then
		writeln('Yes')
	else
		writeln('No');
end.

```
