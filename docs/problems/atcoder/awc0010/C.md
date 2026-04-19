# Problem: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, j, k, q, l, r: int32;
	a, area: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k, q);

	read(a[1]);
	area[1] := 1;

	for i := 2 to n do begin
		read(a[i]);
		if abs(a[i] - a[i-1]) <= k then
			area[i] := area[i-1]
		else
			area[i] := i;
	end;
	readln;

	for j := 1 to q do begin
		readln(l, r);
		if area[l] = area[r] then
			writeln('Yes')
		else
			writeln('No');
	end;
end.

```
