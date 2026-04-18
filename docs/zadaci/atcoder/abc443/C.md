# Задатак: C.pas

```pascal
program _C;
var
	n, tt, t, i, ai, ans: int32;

begin
	readln(n, tt);

	ans := 0;
	t := 0;

	for i := 1 to n+1 do begin

		if i > n then
			ai := tt
		else
			read(ai);

		if ai >= t then begin
			inc(ans, ai - t);
			t := ai + 100;
		end;

	end;
	readln;

	writeln(ans);
end.

```
