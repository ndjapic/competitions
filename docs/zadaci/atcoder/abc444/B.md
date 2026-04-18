# Задатак: B.pas

```pascal
program _B;
var
	n, k, i, x, c, s: int32;

begin
	readln(n, k);

	c := 0;
	for i := 1 to n do begin
		x := i;
		s := 0;

		while x > 0 do begin
			inc(s, x mod 10);
			x := x div 10;
		end;

		if s = k then inc(c);
	end;

	writeln(c);
end.

```
