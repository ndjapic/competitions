program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i, j, ans: int32;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	readln(t);
	n := length(s);
	m := length(t);

	i := 1;
	j := 1;
	ans := 0;

	while ((i <= n) or (j <= m)) and (ans > -1) do
		if (i <= n) and (j <= m) and (s[i] = t[j]) then begin
			inc(i);
			inc(j);
		end else if (i <= n) and (s[i] = 'A') then begin
			inc(i);
			inc(ans);
		end else if (j <= m) and (t[j] = 'A') then begin
			inc(j);
			inc(ans);
		end else
			ans := -1;

	writeln(ans);
end.
