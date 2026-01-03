program C_Pacer;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i, m, d, ans: int32;
	a: array [0 .. nn] of int32;
	b: array [0 .. nn] of int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, m);

		ans := 0;
		a[0] := 0;
		b[0] := 0;

		for i := 1 to n do begin
			readln(a[i], b[i]);
			d := a[i] - a[i-1];
			if b[i] = b[i-1] then
				inc(ans, d - d mod 2)
			else
				inc(ans, d - 1 + d mod 2);
		end;

		writeln(ans + m - a[n]);

	end;
end.
