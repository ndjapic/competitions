program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 3000;
	cc = 10 * 1000;
	inf = 1 shl 30;
var
	n, s, i, j, v, c: int32;
	dp: array [0 .. cc] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, s);

	for j := 1 to s do dp[j] := -inf;
	dp[0] := 0;

	for i := 1 to n do begin
		readln(v, c);

		for j := s downto c do
			dp[j] := max(dp[j], dp[j-c] + v);
	end;

	if dp[s] <= 0 then dp[s] := -1;
	writeln(dp[s]);
end.
