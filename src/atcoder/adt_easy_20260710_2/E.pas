program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i: int32;
	k: int8;
	ans: boolean;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(k);
	readln(s);
	readln(t);
	n := length(s);
	m := length(t);

	ans := false;
	if m - n = 1 then begin

		i := 1;
		while (i <= n) and (s[i] = t[i]) do inc(i);
		while (i <= n) and (s[i] = t[i+1]) do inc(i);
		ans := i > n;

	end else if n - m = 1 then begin

		i := 1;
		while (i <= m) and (t[i] = s[i]) do inc(i);
		while (i <= m) and (t[i] = s[i+1]) do inc(i);
		ans := i > m;

	end else if m = n then begin

		i := 1;
		while (i <= n) and (s[i] = t[i]) do inc(i);
		inc(i);
		while (i <= n) and (s[i] = t[i]) do inc(i);
		ans := i > n;

	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
