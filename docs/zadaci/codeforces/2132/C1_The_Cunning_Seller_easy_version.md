# Задатак: C1_The_Cunning_Seller_easy_version.pas

```pascal
program C1_The_Cunning_Seller_easy_version;
var
	ntc, tci: int16;
	n, p3, cost: int64;
	e: int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		e := 0;
		p3 := 1;
		cost := 0;

		while n > 0 do begin

			if e = 0 then
				inc(cost, n mod 3 * 3)
			else
				inc(cost, n mod 3 * (9+e) * p3 div 3);

			n := n div 3;
			inc(e);
			p3 := p3 * 3;

		end;

		writeln(cost);

	end;
end.

```
