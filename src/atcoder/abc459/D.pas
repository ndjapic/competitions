program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i: int32;
	s: string;
	ch: char;
	c: array ['a' .. 'z'] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);

	for tci := 1 to notc do begin
		readln(s);
		n := length(s);

		for ch := 'a' to 'z' do c[ch] := 0;
		for ch in s do inc(c[ch]);

		for i := 1 to n do begin
			s[i] := 'a';
			if i > 1 then
				while s[i] = s[i-1] do inc(s[i]);

			for ch := 'a' to 'z' do
				if c[ch] > c[s[i]] then begin
					if (i = 1) or (s[i-1] <> ch) then
						s[i] := ch;
				end;
			dec(c[s[i]]);
		end;

		i := 2;
		while (i <= n) and (s[i-1] <> s[i]) do inc(i);

		ch := 'a';
		while (ch <= 'z') and (c[ch] = 0) do inc(ch);

		if (i > n) and (ch > 'z') then begin
			writeln('Yes');
			writeln(s);
		end else
			writeln('No');
	end;
end.
