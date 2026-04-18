# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1000;
	mm = 3000;
var
	n, m, i, j: int32;
	a, b: array [1 .. nn] of int32;
	dp: array [0 .. nn, 0 .. mm] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for j := 0 to m do dp[0, j] := 0;

	opt := 0;
	for i := 1 to n do begin
		readln(a[i], b[i]);
		for j := 0 to m do begin
			dp[i, j] := dp[i-1, j];
			if j >= a[i] then
				dp[i, j] := max(dp[i, j], dp[i-1, j - a[i]] + b[i]);
			opt := max(opt, dp[i, j]);
		end;
	end;
end.

```
