program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp
uses
	math;
const
	NN = 100;
var
	x, y, n, i: int32;
	dp: array [0 .. NN] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y, n);

	dp[0] := 0;
	for i := 1 to n do begin
		dp[i] := dp[i-1] + x;
		if i >= 3 then dp[i] := min(dp[i], dp[i-3] + y)
	end;

	writeln(dp[n]);
end.
