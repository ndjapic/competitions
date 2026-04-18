# Задатак: B_Expansion_Plan_2.pas

```pascal
program B_Expansion_Plan_2;
var
	notc, tci, n, i, m, x, y: int32;
	s: string;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(n, x, y);
		readln(s);

		m := 0;
		for i := 1 to n do
			if s[i] = '8' then inc(m);

		x := abs(x);
		y := abs(y);

		if x > n then
			writeln('NO')
		else if y > n then
			writeln('NO')
		else if x+y > n+m then
			writeln('NO')
		else
			writeln('YES');
	end;
end.

```
