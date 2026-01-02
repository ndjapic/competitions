program A_Operations_with_Inversions;
var
	notc, tci, n, i, c, x, m: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		c := 0;
		m := 0;
		for i := 1 to n do begin
			read(x);
			if x < m then
				inc(c)
			else
				m := x;
		end;
		readln;

		writeln(c);

	end;
end.
