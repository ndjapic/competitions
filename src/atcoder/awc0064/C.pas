program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp #knapsack
uses
	math;
const
	nn = 100;
	mm = 100 * 1000;
var
	n, i: int8;
	m, j, r, t: int32;
	ans: int64;
	dp: array [0 .. nn, 0 .. mm] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 0 to m do dp[0, j] := 0;

	for i := 1 to n do begin
		readln(r, t);
		for j := 0 to m do dp[i, j] := dp[i-1, j];
		for j := t to m do dp[i, j] := max(dp[i, j], dp[i-1, j-t] + r);
	end;

	ans := 0;
	for j := 0 to m do ans := max(ans, dp[n, j]);

	writeln(ans);
end.
