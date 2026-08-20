program B_Substring_2;
uses
	math;
var
	n, m, i, j, l, r, op, ans: int32;
	s, t: string;

begin
	readln(n, m);
	readln(s);
	readln(t);

	l := 0;
	ans := 1 shl 30;
	for r := m to n do begin
		op := 0;
		for j := 1 to m do begin
			i := l+j;
			inc(op, (ord(s[i]) + 10 - ord(t[j])) mod 10);
		end;
		ans := min(ans, op);
		inc(l);
	end;

	writeln(ans);
end.
