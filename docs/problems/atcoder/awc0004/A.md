# Problem: A.pas

```pascal
program _A;
var
	n, i, s, t, ai, m: int16;

begin
	readln(n, s, t);

	m := 0;
	for i := 1 to n do begin
		read(ai);
		inc(m, ai);
	end;
	readln;

	if m <= (t-s) * 60 then
		writeln('Yes')
	else
		writeln('No');
end.

```
