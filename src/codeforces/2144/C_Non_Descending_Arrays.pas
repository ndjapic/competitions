program C_Non_Descending_Arrays;
const
	nn = 100;
	prime = 998244353;
var
	notc, tci, n, i, x, ans: int32;
	a, b: array [1 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n do read(a[i]); readln;
		for i := 1 to n do read(b[i]); readln;

		for i := 1 to n do
			if a[i] > b[i] then begin
				x := a[i];
				a[i] := b[i];
				b[i] := x;
			end;

		ans := 2;
		for i := 2 to n do
			if (a[i-1] <= b[i]) and (b[i-1] <= a[i]) then begin
				inc(ans, ans);
				if ans >= prime then dec(ans, prime);
			end;

		writeln(ans);

	end;
end.
