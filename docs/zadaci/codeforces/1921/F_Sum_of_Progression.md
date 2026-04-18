# Задатак: F_Sum_of_Progression.pas

```pascal
program F_Sum_of_Progression;
const
    maxn = 100 * 1000;
    maxsq = 140;
var
	ntc, tci: int16;
    n, q, i, j, k: int32;
    s, d, ans: int64;
    a: array [1 .. maxn] of int32;
    b, c: array [1 .. maxn, 1 .. maxsq] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, q);

		for i := 1 to n do read(a[i]);
		readln;

		d := 1;
		while (d <= maxsq) and (n-d >= 0) do begin

			for i := n downto n-d+1 do begin
				b[i, d] := a[i];
				c[i, d] := a[i];
			end;

			for i := n-d downto 1 do begin
                b[i, d] := a[i] + b[i+d, d];
                c[i, d] := b[i, d] + c[i+d, d];
			end;

			inc(d);
		end;

		for i := 1 to q do begin

			readln(s, d, k);

			if d <= maxsq then begin
				ans := c[s, d];
				inc(s, d*k);
				if s <= n then dec(ans, c[s, d] + b[s, d] * k);
			end else begin
				ans := 0;
				for j := 1 to k do inc(ans, int64(a[s + d*(j-1)]) * j);
			end;

			write(ans);
			if i < q then write(' ');

		end;
		writeln;

    end;
end.

```
