# Problem: D_From_1_to_Infinity.pas

```pascal
program D_From_1_to_Infinity;
var
	ntc, tci: int16;
	k, l, r, p10: int64;
	e: int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(k);

		p10 := 1;
		e := 1;
		l := 0;
		r := 9;

		while r < k do begin
			p10 := p10 * 10;
			inc(e);
			l := r;
			inc(r, p10 * 9 * e);
		end;

		(k-l) div e

	end;
end.

```
