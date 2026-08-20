program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp #naive #TLE
uses
	math;
const
	NN = 200 * 1000;
var
	n, i, j, k: int32;
	ans: int64;
	a: array [1 .. NN] of int32;
	dp: array [1 .. NN] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	k := min(k, (n-1) div 2);
	read(a[1]);
	dp[1] := a[1];

	for i := 2 to n do begin
		read(a[i]);
		dp[i] := dp[i-1] + a[i];
	end;
	readln;

	for j := 1 to k do
		for i := n downto 2*j+1 do begin
			dp[i] := max(dp[i], dp[i-2] + a[i]);
			if i < n then
				dp[i+1] := max(dp[i+1], dp[i] + a[i+1]);
		end;

	ans := dp[n];
	writeln(ans);
end.
