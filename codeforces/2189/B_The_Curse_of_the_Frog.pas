program B_The_Curse_of_the_Frog;
uses
	math;
const
	nn = 100 * 1000;
	inf = int64(1) shl 60;
var
	notc, tci, n, i: int32;
	x, a, b, c, k: int64;
	d: array [1 .. nn] of int64;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, x);
		dec(x);

		for i := 1 to n do begin
			read(a, b, c);
			dec(x, (b-1)*a);
			d[i] := a*b-c;
		end;
		readln;

		if x < 0 then
			k := 0
		else begin
			k := inf;
			for i := 1 to n do begin
				if d[i] > 0 then
					k := min(k, x div d[i] + 1);
			end;
		end;

		if k >= inf then k := -1;
		writeln(k);

	end;
end.
