program B_Postal_Card;
var
	n, m, i, j, ans: int16;
	s: array [1 .. 1000, 1 .. 6] of char;
	t: array [1 .. 1000, 1 .. 3] of char;

begin
	readln(n, m);

	for i := 1 to n do begin
		read(s[i, 1]);
		read(s[i, 2]);
		read(s[i, 3]);
		read(s[i, 4]);
		read(s[i, 5]);
		readln(s[i, 6]);
	end;

	for j := 1 to m do begin
		read(t[j, 1]);
		read(t[j, 2]);
		readln(t[j, 3]);
	end;

	ans := 0;
	for i := 1 to n do begin

		j := 1;
		while (j <= m) and ((s[i, 4] <> t[j, 1]) or (s[i, 5] <> t[j, 2]) or (s[i, 6] <> t[j, 3])) do inc(j);
		if j <= m then inc(ans);

	end;

	writeln(ans);

end.
