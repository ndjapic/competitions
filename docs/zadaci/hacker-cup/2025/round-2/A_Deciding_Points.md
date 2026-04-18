# Задатак: A_Deciding_Points.pas

```pascal
program A_Deciding_Points;
uses
	math;
var
	notc, tci, n, m: int32;
	ans: boolean;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(n, m);
		m := max(2, m);

		if n < m then
			ans := false
		else if n <= 2*m-2 then
			ans := true
		else if odd(n) then
			ans := false
		else
			ans := true;

		write('Case #', tci, ': ');
		if ans then
			writeln('YES')
		else
			writeln('NO');
	end;
end.

```
