program C_War_Strategy;
uses
	math;
const
	nn = 200 * 1000;
var
	notc, tci, n, m, k, a, b, c, l, r: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m, k);
		k := min(k, n+1-k);

		if k = 1 then
			writeln(min(n, (m+1) div 2))
		else begin
			{3*d-2 <= m}

			a := 1;
			b := k;
			while b-a > 1 do begin
				c := (a+b) div 2;
				if 3*c-2 <= m then
					a := c
				else
					b := c;
			end;

			l := k-a;
			dec(m, 2*a-1);

			a := 1;
			b := n+1-k;
			while b-a > 1 do begin
				c := (a+b) div 2;
				if max(0, c-(k-l)) + c <= m then
					a := c
				else
					b := c;
			end;

			r := k+c;
			writeln(r-l+1);

		end;

	end;
end.
