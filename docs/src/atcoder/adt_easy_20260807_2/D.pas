program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp #learn
uses
	math;
const
	NN = 100;
	INF = 1000 * 1000;
var
	n, i, j: int8;
	s, m, l: int32;
	dp: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, m, l);

	for i := 1 to n do begin
		dp[i] := INF;

		for j := max(i-6, 0) to i-1 do
			dp[i] := min(dp[i], dp[j] + s);

		for j := max(i-8, 0) to i-1 do
			dp[i] := min(dp[i], dp[j] + m);

		for j := max(i-12, 0) to i-1 do
			dp[i] := min(dp[i], dp[j] + l);
	end;

	writeln(dp[n]);
end.
