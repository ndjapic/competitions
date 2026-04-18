program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, l, r, ans: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	readln(s);
	readln(t);

	l := 1;
	r := n;
	ans := 0;

	while (l <= n) and (s[l] = t[l]) do inc(l);
	while (r > 0) and (s[r] = t[r-n+m]) do dec(r);
	if r > 0 then inc(ans, 1);
	if l <= n then inc(ans, 2);

	writeln(ans);
end.
