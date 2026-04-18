program A_Dungeon_Equilibrium;
uses
	math;
const
	nn = 100;
var
	notc, tci, n, i, x, ans: int16;
	c: array [0 .. nn] of int8;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(n);

		for x := 0 to n do c[x] := 0;

		for i := 1 to n do begin
			read(x);
			inc(c[x]);
		end;
		readln;

		ans := 0;
		for x := 0 to n do
			if c[x] < x then
				inc(ans, c[x])
			else
				inc(ans, c[x] - x);

		writeln(ans);
	end;
end.
