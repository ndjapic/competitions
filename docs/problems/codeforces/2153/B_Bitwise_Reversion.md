# Problem: B_Bitwise_Reversion.pas

```pascal
program B_Bitwise_Reversion;
var
	notc, tci, x, y, z, a, b, c, all: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(x, y, z);

		all := x and y and z;

		if x and y <> all then
			writeln('NO')
		else if y and z <> all then
			writeln('NO')
		else if z and x <> all then
			writeln('NO')
		else
			writeln('YES');

	end;
end.

```
