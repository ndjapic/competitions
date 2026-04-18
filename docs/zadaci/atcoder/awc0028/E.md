# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100 * 1000;
	kk = 100;
	prime = 1000 * 1000 * 1000 + 7;
var
	n, k, i, j: int32;
	a: array [1 .. nn] of int32;
	p: array [1 .. kk] of int32;
	dp: array [0 .. nn, 0 .. kk] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(a[i]); readln;
	for j := 1 to k do read(p[j]); readln;

	for j := 1 to k do dp[0, j] := 0;
	dp[0, 0] := 1;

	for i := 1 to n do begin
		dp[i, 0] := 1;
		for j := 1 to k do begin
			dp[i, j] := dp[i-1, j];
			if a[i] = p[j] then begin
				inc(dp[i, j], dp[i-1, j-1]);
				if dp[i, j] >= prime then
					dec(dp[i, j], prime);
			end;
		end;
	end;

	writeln(dp[n, k]);
end.

```
