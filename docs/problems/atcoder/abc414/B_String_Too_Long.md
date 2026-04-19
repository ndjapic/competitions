# Problem: B_String_Too_Long.pas

```pascal
program B_String_Too_Long;
const
	nn = 100;
var
	n, i, j: int8;
	s: int64;
	c: array [1 .. nn] of char;
	l: array [1 .. nn] of int64;

begin
	readln(n);

	for i := 1 to n do readln(c[i], l[i]);

	s := 0;
	i := 1;
	while (i <= n) and (s <= 100) do begin
		inc(s, l[i]);
		inc(i);
	end;

	if s > 100 then
		writeln('Too Long')
	else begin
		for i := 1 to n do
			for j := 1 to l[i] do write(c[i]);
		writeln;
	end;
end.

```
