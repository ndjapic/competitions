# Задатак: C_Leftmost_Below.pas

```pascal
program C_Leftmost_Below;
uses
	math;
const
	nn = 200 * 1000;
	inf = 1 shl 30;
var
	ntc, tci, n, i: int32;
	b, pre: array [0 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		pre[0] := inf;
		for i := 1 to n do begin
			read(b[i]);
			pre[i] := min(pre[i-1], b[i]);
		end;
		readln;

		i := n;
		while (i > 0) and (b[i] < 2 * pre[i-1]) do dec(i);

		if i > 0 then
			writeln('NO')
		else
			writeln('YES');

	end;
end.

```
