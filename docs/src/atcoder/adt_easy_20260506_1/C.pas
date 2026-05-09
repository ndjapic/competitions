program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp
uses
	math;
const
	nn = 100;
	inf = 1 shl 20;
var
	n, i, s, m, l: int32;
	dp: array [-12 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s, m, l);

	for i := -12 to 0 do
		dp[i] := 0;

	for i := 1 to n do begin
		dp[i] := inf;
		dp[i] := min(dp[i], dp[i-6] + s);
		dp[i] := min(dp[i], dp[i-8] + m);
		dp[i] := min(dp[i], dp[i-12] + l);
	end;

	writeln(dp[n]);
end.
