# Задатак: C_Needle_in_a_Haystack.pas

```pascal
program C_Needle_in_a_Haystack;
const
	nn = 100 * 1000;
var
	notc, tci, n, m, i, j: int32;
	ch: char;
	s, t: string;
	c: array ['a' .. 'z'] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(s);
		readln(t);
		n := length(s);
		m := length(t);

		for ch := 'a' to 'z' do c[ch] := 0;
		for j := 1 to m do inc(c[t[j]]);
		for i := 1 to n do dec(c[s[i]]);

		ch := 'a';
		while (ch <= 'z') and (c[ch] >= 0) do inc(ch);

		if ch <= 'z' then
			writeln('Impossible')
		else begin

			j := 1;
			for i := 1 to n do begin
				for ch := 'a' to s[i] do
					if ch < s[i] then
						while c[ch] > 0 do begin
							t[j] := ch;
							dec(c[ch]);
							inc(j);
						end;

				t[j] := s[i];
				inc(j);
			end;

			for ch := 'a' to 'z' do
				while c[ch] > 0 do begin
					t[j] := ch;
					dec(c[ch]);
					inc(j);
				end;

			writeln(t);

		end;

	end;
end.

```
