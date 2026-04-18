# Задатак: A_Redstone.pas

```pascal
program A_Redstone;
var
	ntc, tci: int16;
	n, i, ai: int8;
	c: array [1 .. 100] of int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for ai := 1 to 100 do c[ai] := 0;

		for i := 1 to n do begin
			read(ai);
			inc(c[ai]);
		end;
		readln;

		ai := 1;
		while (ai <= 100) and (c[ai] < 2) do inc(ai);

		if ai <= 100 then
			writeln('YES')
		else
			writeln('NO');

	end;
end.

```
