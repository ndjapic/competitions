# Problem: B.pas

```pascal
program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, i: int32;
	h, d, mn, dp: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do read(h[i]); readln;
	for i := 1 to n do read(d[i]); readln;
	for i := 1 to n do mn[i] := min(h[i], d[i]);

	dp[1] := 0;
	if mn[1] > 0 then inc(dp[1]);
	dp[2] := dp[1];
	if mn[2] > 0 then inc(dp[2]);

	for i := 3 to n do begin
		dp[i] := min(dp[i-2], dp[i-1]);
		if mn[i] > 0 then inc(dp[i]);
	end;

	writeln(dp[n]);
end.

```
