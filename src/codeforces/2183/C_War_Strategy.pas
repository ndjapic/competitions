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
			writeln(min(n, (m+1) div 2 + 1))
		else if m <= 2 then
			writeln(min(n, m+1))
		else begin
			{c+c-1+c <= m}
			{3*c <= m+1}
			{c <= (m+1) div 3}

			a := 1;
			b := min(k, (m+1) div 3 + 1);
			while b-a > 1 do begin
				c := (a+b) div 2;
				if 3*c-1 <= m then
					a := c
				else
					b := c;
			end;

			l := k-a;
			dec(m, 2*a-1);

			a := 1;
			b := min(m+2, n-k+1);
			while b-a > 1 do begin
				c := (a+b) div 2;
				if max(0, c-(k-l)) + c <= m then
					a := c
				else
					b := c;
			end;

			r := k+a;
			writeln(r-l+1);

		end;

	end;
end.
