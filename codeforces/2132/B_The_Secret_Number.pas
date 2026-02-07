program B_The_Secret_Number;
const
	inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
	ntc, tci: int16;
	n, p10: int64;
	e, k, i: int8;
	ans: array [1 .. 18] of int64;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		p10 := 1;
		e := 0;
		k := 0;

		while e < 18 do begin
			inc(e);
			p10 := p10 * 10;

			if n mod (p10 + 1) = 0 then begin
				inc(k);
				ans[k] := n div (p10 + 1);
			end;
		end;

		writeln(k);
		if k > 0 then begin
			for i := k downto 2 do write(ans[i], ' ');
			writeln(ans[1]);
		end;

	end;
end.
