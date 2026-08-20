program D_Match_or_Not;
const
	maxn = 300 * 1000;
var
	n, m, i, x, c: int32;
	s, t: array [1 .. maxn] of char;
	match: array [1 .. maxn] of boolean;

begin

	n := 0;
	repeat
		inc(n);
		read(s[n]);
	until eoln;
	readln;

	m := 0;
	repeat
		inc(m);
		read(t[m]);
	until eoln;
	readln;

	c := 0;
	i := n;

	for x := m downto 1 do begin
		match[x] := (t[x] = s[i]) or (t[x] = '?') or (s[i] = '?');
		if match[x] then inc(c);
		dec(i);
	end;

	if c = m then
		writeln('Yes')
	else
		writeln('No');

	for x := 1 to m do begin

		if match[x] then dec(c);
		match[x] := (t[x] = s[x]) or (t[x] = '?') or (s[x] = '?');
		if match[x] then inc(c);

		if c = m then
			writeln('Yes')
		else
			writeln('No');

	end;

end.
