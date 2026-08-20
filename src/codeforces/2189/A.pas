program A;
uses
	math;
var
	notc, tci, n, i, h, l, mn, mx, x, c1, c2, ans: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, h, l);
		mn := min(h, l);
		mx := max(h, l);

		c1 := 0;
		c2 := 0;
		for i := 1 to n do begin
			read(x);
			if x <= mn then
				inc(c1)
			else if x <= mx then
				inc(c2);
		end;
		readln;

		mn := min(c1, c2);
		ans := mn + (c1-mn) div 2;
		writeln(ans);

	end;
end.
