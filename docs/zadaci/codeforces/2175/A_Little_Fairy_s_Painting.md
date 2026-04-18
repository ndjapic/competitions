# Задатак: A_Little_Fairy_s_Painting.pas

```pascal
program A_Little_Fairy_s_Painting;
uses
	math;
const
	aa = 1001;
var
	notc, tci, n, i, x, d: int32;
	c: array [1 .. aa] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for x := 1 to aa do c[x] := 0;
		d := 0;

		for i := 1 to n do begin
			read(x);
			inc(c[x]);
			if c[x] = 1 then inc(d);
		end;
		readln;

		while c[d] = 0 do begin
			inc(c[d]);
			inc(d);
		end;

		writeln(d);

	end;
end.

```
