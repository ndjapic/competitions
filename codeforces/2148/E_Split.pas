program E_Split;
const
	nn = 200 * 1000;
var
	ntc, tci, n, k, i, v, l, r: int32;
	ans: int64;
	a, c: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);

		for v := 1 to n do c[v] := 0;

		for i := 1 to n do begin
			read(v);
			a[i] := v;
			inc(c[v]);
		end;
		readln;

		v := 1;
		while (v <= n) and (c[v] mod k = 0) do begin
			c[v] := c[v] div k;
			inc(v);
		end;

		ans := 0;
		if v > n then begin

			l := 0;
			for r := 1 to n do begin
				v := a[r];
				dec(c[v]);

				while c[v] < 0 do begin
					inc(l);
					inc(c[a[l]]);
				end;

				inc(ans, r-l);
			end;

		end;

		writeln(ans);

	end;
end.
