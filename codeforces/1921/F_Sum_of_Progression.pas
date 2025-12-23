program F_Sum_of_Progression;
const
    maxn = 200 * 1000;
var
	ntc, tci: int16;
    n, q, i, j, s, d, k: int32;
    ans: int64;
    a: array [1 .. maxn] of int32;
    b, c: array [1 .. maxn, 1 .. 450] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, q);

		for i := 1 to n do read(a[i]);
		readln;

		d := 1;
		while d*d <= n do begin

			for i := n downto 1 do begin

				b[i, d] := a[i];
				c[i, d] := a[i];

				s := i-d;
				while s > 0 do begin
					b[s, d] := a[s] + b[s+d, d];
					c[s, d] := b[s, d] + c[s+d, d];
					dec(s, d);
				end;

			end;

			inc(d);
		end;

		for i := 1 to q do begin

			readln(s, d, k);

			if d*d <= n then begin
				ans := c[s, d];
				inc(s, d*k);
				if s <= n then dec(ans, c[s, d] + b[s, d] * k);
			end else begin
				ans := 0;
				for j := 1 to k do inc(ans, a[s + d*(j-1)] * j);
			end;

			write(ans);
			if i < q then write(' ');

		end;
		writeln;

    end;
end.
