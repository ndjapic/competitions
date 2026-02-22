program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200;
var
	j, k, m, n, l, r: int32;
	d, ans: int64;
	a, b: array [1 .. nn] of int32;
	dp: array [0 .. nn, 0 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	for j := 0 to m do dp[0, j] := 0;

	ans := 0;
	for r := 1 to n do begin
		readln(a[r], b[r]);
		d := a[r] - b[r];
		for j := b[r] to m do begin
			dp[r, j] := 0;
			for l := max(0, r-k) to r-1 do
				dp[r, j] := max(dp[r, j], dp[l, j-b[r]] + d);
			ans := max(ans, dp[r, j]);
		end;
	end;

	writeln(ans);
end.
