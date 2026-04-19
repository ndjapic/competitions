# Problem: A_Square.pas

```pascal
program A_Square;
var
	notc, tci, a, b, c, d: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(a, b, c, d);

		if a <> b then
			writeln('NO')
		else if b <> c then
			writeln('NO')
		else if c <> d then
			writeln('NO')
		else
			writeln('YES');

	end;
end.

```
