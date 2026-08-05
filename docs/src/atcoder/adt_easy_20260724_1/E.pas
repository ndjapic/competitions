program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp
uses
	math;
const
	NN = 50;
	KK = NN * NN;
	PRIME = 998244353;
var
	n, m, i, j, k, a: int32;
	ans: int64;
	dp: array [0 .. NN, 0 .. KK] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);
	dec(m);
	dec(k, n);

	for j := 1 to k do dp[0, j] := 0;
	dp[0, 0] := 1;

	for i := 1 to n do
		for j := 0 to k do begin
			dp[i, j] := 0;
			for a := 0 to min(m, j) do
				inc(dp[i, j], dp[i-1, j-a]);
			dp[i, j] := dp[i, j] mod PRIME;
		end;

	ans := 0;
	for j := 0 to k do
		inc(ans, dp[n, j]);

	writeln(ans mod PRIME);
end.
