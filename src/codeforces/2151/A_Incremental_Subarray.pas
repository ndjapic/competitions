program A_Incremental_Subarray;
const
	nn = 100 * 1000;
	mm = 200;
var
	notc, tci, n, m, i: int32;
	a: array [1 .. mm] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n, m);

		for i := 1 to m do begin
			read(a[i]);
		end;
		readln;

		i := 2;
		while (i <= m) and (a[i-1] < a[i]) do inc(i);

		if i <= m then
			writeln('1')
		else
			writeln(n+1-a[m]);

	end;
end.
