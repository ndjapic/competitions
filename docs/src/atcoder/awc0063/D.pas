program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #dp #segment
uses
	math;
const
	nn = 3000;
var
	n, l, r: int32;
	dp: array [1 .. nn, 1 .. nn] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for r := 1 to n do begin
		read(dp[r, r]);
		for l := r-1 downto 1 do
			dp[l, r] := max(dp[r, r] - dp[l, r-1], dp[l, l] - dp[l+1, r]);
	end;
	readln;

	writeln(dp[1, n])
end.
