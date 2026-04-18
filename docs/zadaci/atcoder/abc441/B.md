# Задатак: B.pas

```pascal
program B;
var
	n, m, q, i, j, k: int32;
	s, t, w: string;
	ch: char;
	lang: array ['a' .. 'z'] of char;

begin
	readln(n, m);
	readln(s);
	readln(t);

	for ch := 'a' to 'z' do lang[ch] := 'x';

	for i := 1 to n do lang[s[i]] := 't';

	for i := 1 to m do
		if lang[t[i]] = 'x' then
			lang[t[i]] := 'a'
		else
			lang[t[i]] := '?';

	readln(q);
	for i := 1 to q do begin
		readln(w);
		k := length(w);

		j := 1;
		while (j <= k) and (lang[w[j]] <> 't') do inc(j);
		if j <= k then
			writeln('Takahashi')
		else begin

			j := 1;
			while (j <= k) and (lang[w[j]] <> 'a') do inc(j);
			if j <= k then
				writeln('Aoki')
			else
				writeln('Unknown');

		end;
	end;
end.

```
