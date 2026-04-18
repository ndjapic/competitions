program D_Christmas_Tree_Decoration;
uses
	math;
const
	nn = 50;
	prime = 998244353;
var
	notc, tci, n, i, s, d, m, c, r, ans: int32;
	a: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		s := 0;
		for i := 0 to n do begin
			read(a[i]);
			inc(s, a[i]);
		end;
		readln;

		d := s div n;
		m := s mod n;

		i := 1;
		while (i <= n) and (a[i] <= d+1) do inc(i);

		if i <= n then
			writeln(0)
		else begin

			c := 0;
			for i := 1 to n do
				if a[i] <= d then inc(c);

			ans := 1;
			r := 0;
			for i := 1 to n do begin
				inc(r);
				if i = m+1 then dec(r, n-c);
				ans := int64(r) * ans mod prime;
			end;

			writeln(ans);

		end;

	end;
end.
