program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp
uses
	math;
const
	nn = 3000;
	inf = int64(1) shl 32;
var
	n, d, i, j, f: int32;
	c: int64;
	dp: array [1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, d);

	for j := 1 to d do dp[j] := inf;

	for i := 1 to n do begin
		readln(c, f);

		for j := 1 to min(f, d) do
			dp[j] := min(dp[j], c);

		for j := f+1 to d do
			dp[j] := min(dp[j], dp[j-f] + c);
	end;

	writeln(dp[d]);
end.
