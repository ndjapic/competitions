program B_New_Year_Cake;
uses
	math;
var
	notc, tci, a, b, x, y, e0, e1, p2: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(a, b);

		e0 := 0;
		p2 := 1;
		x := p2;
		y := 0;

		while (x <= a) and (y <= b) do begin
			inc(e0);
			p2 := p2 * 2;
			if odd(e0) then
				inc(y, p2)
			else
				inc(x, p2);
		end;

		e1 := 0;
		p2 := 1;
		x := 0;
		y := p2;

		while (x <= a) and (y <= b) do begin
			inc(e1);
			p2 := p2 * 2;
			if odd(e1) then
				inc(x, p2)
			else
				inc(y, p2);
		end;

		writeln(max(e0, e1));

	end;
end.
