# Задатак: B_Addition_on_a_Segment.pas

```pascal
program B_Addition_on_a_Segment;
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, i, c: int32;
	s: int64;
	b: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		c := 0;
		s := 0;
		for i := 1 to n do begin
			read(b[i]);
			inc(c, min(b[i], 1));
			inc(s, b[i]);
		end;
		readln;

		dec(s, c);
		dec(c, max(0, n-1-s));
		writeln(c);

	end;
end.

```
