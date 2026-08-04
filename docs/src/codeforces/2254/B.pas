program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	notc, tci, n, i, ans: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		ans := 1;
		for i := 2 to n do
			if s[i] <> s[i-1] then inc(ans);

		i := 2;
		while (i < n) and not ((s[i-1] <> s[i]) and (s[i] <> s[i+1]) and (s[i-1] = s[i+1])) do inc(i);

		if i < n then
			dec(ans, 2)
		else begin

			i := 2;
			while (i < n) and not ((s[i-1] <> s[i]) and (s[i] <> s[i+1])) do inc(i);

			if i < n then dec(ans);

		end;

		writeln(ans);

	end;
end.
