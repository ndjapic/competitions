# Задатак: G_Modular_Sorting.pas

```pascal
program G_Modular_Sorting;
const
	nn = 100 * 1000;
var
	ntc, tci, n, m, q, i, j, k, x, c, d: int32;
	tp: int8;
	a, b: array [1 .. nn] of int32;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, m, q);

		for i := 1 to n do read(a[i]); readln;

		for j := 1 to q do begin
			read(tp);
			case tp of

				1: begin
					readln(i, x);
					a[i] := x;
				end;

				2: begin
					readln(k);

					k := gcd(k, m);
					d := m div k;
					b[1] := a[1] mod k;
					c := 0;
					i := 2;

					while (i <= n) and (c < d) do begin
						b[i] := a[i] mod k;
						if b[i-1] > b[i] then inc(c);
						inc(i);
					end;

					if c < d then
						writeln('YES')
					else
						writeln('NO');
				end;

			end;
		end;

	end;
end.

```
