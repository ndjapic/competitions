program C2_The_Cunning_Seller_hard_version;
uses
	math;
var
	ntc, tci: int16;
	n, k, p3, cost, mn: int64;
	e, i: int8;
	digits: array [0 .. 20] of int64;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);

		e := -1;
		while n > 0 do begin
			inc(e);
			digits[e] := n mod 3;
			dec(k, digits[e]);
			n := n div 3;
		end;

		if k < 0 then
			writeln(-1)
		else begin

			for i := e downto 1 do begin
				mn := min(k div 2, digits[i]);
				dec(k, mn * 2);
				dec(digits[i], mn);
				inc(digits[i-1], mn * 3);
			end;

			cost := 0;
			p3 := 1;
			for i := 0 to e do begin
				if i = 0 then
					inc(cost, digits[i] * 3)
				else
					inc(cost, digits[i] * (9+i) * p3 div 3);
				p3 := p3 * 3;
			end;

			writeln(cost);

		end;

	end;
end.
