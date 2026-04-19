# Problem: A_In_the_Dream.pas

```pascal
program A_In_the_Dream;
var
	ntc, tci, a, b, c, d: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(a, b, c, d);

		dec(c, a);
		dec(d, b);

		if a-2 > 2*b then
			writeln('NO')
		else if b-2 > 2*a then
			writeln('NO')
		else if c-2 > 2*d then
			writeln('NO')
		else if d-2 > 2*c then
			writeln('NO')
		else
			writeln('YES');

	end;
end.

```
