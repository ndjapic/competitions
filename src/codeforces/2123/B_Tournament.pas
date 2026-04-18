program B_Tournament;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i, j, k, x: int32;
	a, c: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, j, k);

		for x := 1 to n do c[x] := 0;

		for i := 1 to n do begin
			read(x);
			a[i] := x;
			inc(c[x]);
		end;
		readln;

		if k > 1 then
			writeln('YES')
		else begin
			x := n;
			while c[x] = 0 do
				dec(x);
			if a[j] = x then
				writeln('YES')
			else
				writeln('NO');
		end;

	end;
end.
