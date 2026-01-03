program A_Recycling_Center;
var
	ntc, tci, n, i, ai, ans: int16;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);
		ans := 0;

		for i := 0 to n-1 do begin
			read(ai);
			inc(ans, ai);
			if ai = 0 then inc(ans);
		end;
		readln;

		writeln(ans);

	end;
end.
