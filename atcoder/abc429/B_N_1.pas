program B_N_1;
const
	nn = 100;
var
	n, i: int8;
	m, s: int16;
	a: array [1 .. nn] of int8;

begin
	readln(n, m);

	s := 0;
	for i := 1 to n do begin
		read(a[i]);
		inc(s, a[i]);
	end;
	readln;

	i := 1;
	while (i <= n) and (s <> m + a[i]) do inc(i);

	if i <= n then
		writeln('Yes')
	else
		writeln('No');
end.
