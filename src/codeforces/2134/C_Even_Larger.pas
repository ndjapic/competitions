program C_Even_Larger;
const
	nn = 200 * 1000;
var
	ntc, tci: int16;
	n, i, j, d, s: int32;
	ans: int64;
	a: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		for i := 1 to n do read(a[i]);
		readln;

		ans := 0;
		for i := 2 to n do
			if odd(i) then begin
				d := a[i] - a[i-1];
				if d > 0 then begin
					inc(ans, d);
					dec(a[i], d);
				end;
			end else begin
				d := a[i-1] - a[i];
				if d > 0 then begin
					dec(a[i-1], d);
					inc(ans, d);
				end;
			end;

		i := 0;
		for j := 1 to n div 2 do begin
			inc(i, 2);
			s := a[i-1];
			if i < n then inc(s, a[i+1]);
			d := s - a[i];
			if d > 0 then begin
				dec(a[i+1], d);
				inc(ans, d);
			end;
		end;

		writeln(ans);

	end;
end.
