# Задатак: B.pas

```pascal
program B;
uses
	math;
var
	notc, tci, n, i, ai, mx: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		mx := 0;
		for i := 1 to n do begin
			read(ai);
			mx := max(mx, ai);
		end;
		readln;

		writeln(n * mx);

	end;
end.

```
