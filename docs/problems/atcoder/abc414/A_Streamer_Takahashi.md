# Problem: A_Streamer_Takahashi.pas

```pascal
program A_Streamer_Takahashi;
var
	n, i, l, r, x, y, ans: int32;

begin
	readln(n, l, r);

	ans := 0;
	for i := 1 to n do begin
		readln(x, y);
		if (x <= l) and (r <= y) then inc(ans);
	end;

	writeln(ans);
end.

```
