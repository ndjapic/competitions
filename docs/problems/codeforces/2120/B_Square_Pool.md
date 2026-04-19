# Problem: B_Square_Pool.pas

```pascal
program B_Square_Pool;
{$MODE DELPHI}
var
	ntc, tci, n, i, s, dx, dy, x, y, ans: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, s);

		ans := 0;
		for i := 1 to n do begin
			readln(dx, dy, x, y);
			if (dx = dy) and (x = y) then
				inc(ans)
			else if (dx+dy = 0) and (x+y = s) then
				inc(ans);
		end;

		writeln(ans);

	end;
end.

```
