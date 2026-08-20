program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp
uses
	math;
const
	NN = 200 * 1000;
	KK = 20;
var
	n, k, i, a, b: int32;
	j: int8;
	ans: int64;
	dp: array [0 .. NN, 0 .. KK] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for j := 0 to 2*k do dp[0, j] := 0;

	for i := 1 to n do begin
		readln(a, b);

		dp[i, 0] := dp[i-1, 0] + a;
		for j := 1 to 2 * k do
			if odd(j) then
				dp[i, j] := max(dp[i-1, j] + b, dp[i-1, j-1] + a)
			else
				dp[i, j] := max(dp[i-1, j] + a, dp[i-1, j-1] + b);
	end;

	ans := 0;
	for j := 0 to 2*k do
		ans := max(ans, dp[n, j]);

	writeln(ans);
end.
