# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100;
var
	n, d, i, j, l, r, ans: int32;
	s: string;
	occupied: array [1 .. nn] of boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);

	for j := 1 to d do occupied[j] := false;

	for i := 1 to n do begin
		readln(s);
		for j := 1 to d do
			if s[j] = 'x' then occupied[j] := true;
	end;

	ans := 0;
	l := 0;
	for r := 1 to d do
		if occupied[r] then
			l := r
		else
			ans := max(ans, r-l);

	writeln(ans);
end.

```
