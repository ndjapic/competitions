# Problem: A_Be_Positive.pas

```pascal
program A_Be_Positive;
var
	notc, tci: int32;
	n, i, ai: int8;
	c: array [-1 .. 1] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		c[-1] := 0;
		c[0] := 0;

		for i := 1 to n do begin
			read(ai);
			inc(c[ai]);
		end;
		readln;

		writeln(c[0] + c[-1] mod 2 * 2);

	end;
end.

```
