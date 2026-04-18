program C_Production_of_Snowmen;
uses
	math;
const
	nn = 5000;
var
	notc, tci, n, i, j, k, p, q: int32;
	a, b, c: array [0 .. 2*nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 0 to n-1 do read(a[i]); readln;
		for j := 0 to n-1 do read(b[j]); readln;
		for k := 0 to n-1 do read(c[k]); readln;

		for i := 0 to n-1 do begin
			a[i+n] := a[i];
			b[i+n] := b[i];
			c[i+n] := c[i];
		end;

		p := 0;
		q := 0;
		for j := 0 to n-1 do begin

			i := 0;
			while (i < n) and (a[i] < b[i+j]) do inc(i);
			if i = n then inc(p);

			k := 0;
			while (k < n) and (c[k] > b[k+j]) do inc(k);
			if k = n then inc(q);

		end;

		writeln(int64(n)*p*q);

	end;
end.
