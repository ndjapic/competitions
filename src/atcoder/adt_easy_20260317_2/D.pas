program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	prime = 998244353;
var
	a, b, c, d, e, f: int64;
	abc, def, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c, d, e, f);

	abc := ((a mod prime) * (b mod prime) mod prime) * (c mod prime) mod prime;
	def := ((d mod prime) * (e mod prime) mod prime) * (f mod prime) mod prime;

	ans := abc - def;
	if ans < 0 then inc(ans, prime);

	writeln(ans);
end.
