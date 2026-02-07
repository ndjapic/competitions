program B_No_Casino_in_the_Mountains;
var
	ntc, tci, n, k, i, d, ans: int32;
	ai: int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);

		d := 0;
		ans := 0;

		for i := 1 to n do begin
			read(ai);

			case ai of
				0: inc(d);
				1: d := 0;
			end;

			if d = k then begin
				inc(ans);
				d := -1;
			end;
		end;
		readln;

		writeln(ans);

	end;
end.
