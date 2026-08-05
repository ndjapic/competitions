program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, m, i, ans: int8;
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	readln(s);
	readln(t);
	ans := 0;

	i := 1;
	while (i <= n) and (s[i] = t[i]) do inc(i);
	if i <= n then inc(ans, 2);

	i := 1;
	while (i <= n) and (s[i] = t[m-n+i]) do inc(i);
	if i <= n then inc(ans);

	writeln(ans);
end.
