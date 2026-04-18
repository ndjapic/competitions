# Задатак: B_Sum_of_Digits_Sequence.pas

```pascal
program B_Sum_of_Digits_Sequence;
uses
	math;
const
	nn = 10;
	mm = 45;
var
	n, m, i, bu, bv, c, ans: int8;
	mask: int16;
	u, v: array [1 .. mm] of int8;

begin
	readln(n, m);

	for i := 1 to m do readln(u[i], v[i]);

	ans := m;
	for mask := 1 to (int16(1) shl n) - 2 do begin
		c := 0;
		for i := 1 to m do begin
			bu := (mask shr (u[i] - 1)) and 1;
			bv := (mask shr (v[i] - 1)) and 1;
			if bu + bv <> 1 then inc(c);
		end;
		ans := min(ans, c);
	end;

	writeln(ans);
end.

```
