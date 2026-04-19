# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, a, ab: int32;
	ans: int64;
	dp: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	for a := 1 to n do dp[a] := 0;

	for a := 1 to n do begin
		ab := a;
		while ab <= n do begin
			inc(dp[ab]);
			inc(ab, a);
		end;
	end;

	ans := 0;
	for ab := 1 to n-1 do inc(ans, int64(dp[ab]) * dp[n - ab]);

	writeln(ans);
end.

```
